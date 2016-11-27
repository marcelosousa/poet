{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Collapse
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Collapse for interval semantics 
--  For simplicity, a thread is enabled if the control
--  state is non-negative. 
-------------------------------------------------------------------------------
module Domain.Interval.Collapse where

import Control.Monad.State.Lazy
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Domain.Action
import Domain.Interval.Converter
import Domain.Interval.State
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Flow
import Language.SimpleC.Util
import Model.GCS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Debug.Trace as T

type IntGraph = Graph SymId () (IntState,Act) 
type IntGraphs = Graphs SymId () (IntState,Act)
type WItem = (NodeId,EdgeId,NodeId)
type Worklist = [WItem]
type ResultList = [(IntState,Pos,Act)]
type NodeTable = Map NodeId [(IntState,Act)]

data FixState =
  FixState
  {
    fs_mode :: Bool
  , fs_tid  :: TId 
  , fs_cfgs :: IntGraphs
  , fs_symt :: SymbolTable 
  , fs_cfg  :: IntGraph
  , fs_mark :: Set WItem -- The final worklist: every edge in this worklist is a global action 
  }

type FixOp val = State FixState val

showResultList :: ResultList -> String
showResultList l = "Data Flow Information:\n" ++ (snd $ foldr (\(s,p,a) (n,r) -> 
   let s_a = "CFG Node: " ++ show p ++ "\n"
       s_r = s_a ++ show s
   in (n+1, s_r ++ r))  (1,"") l)

instance Collapsible IntState Act where
  collapse b syst@System{..} st tid = 
    let control = controlPart st
        pos = case M.lookup tid control of
          Nothing -> error $ "collapse: tid " ++ show tid ++ " is not control"
          Just p  -> p
        th_cfg_sym = case M.lookup tid (th_states st) of
          Nothing -> error $ "collpase: cant find thread in the state " ++ show tid
          Just th_st -> th_cfg_id th_st 
        th_cfg = case M.lookup th_cfg_sym cfgs of
          Nothing -> error $ "collapse: cant find thread " ++ show th_cfg_sym
          Just cfg -> cfg 
    in T.trace ("collapse with thread " ++ show tid ++ ", position = " ++ show pos) $ 
       fixpt b tid cfgs symt th_cfg pos st

-- @TODO: Receive other options for widening and state splitting
fixpt :: Bool -> TId -> IntGraphs -> SymbolTable -> IntGraph -> Pos -> IntState -> ResultList 
fixpt b tid cfgs symt cfg@Graph{..} pos st =
  -- reset the node table information with the information passed
  let node_table' = M.insert pos [(st,bot_act)] $ M.map (const []) node_table
      cfg' = cfg { node_table = node_table' }
      wlist = map (\(a,b) -> (pos,a,b)) $ succs cfg' pos
      i_fix_st = FixState b tid cfgs symt cfg' S.empty
  in  evalState (worklist wlist) i_fix_st 

add_mark :: WItem -> FixOp ()
add_mark i = do 
  fs@FixState{..} <- get
  let fs_mark' = S.insert i fs_mark
  put fs { fs_mark = fs_mark' }

update_node_table :: NodeTable -> FixOp IntGraph
update_node_table node_table' = do 
  fs@FixState{..} <- get
  let cfg = fs_cfg { node_table = node_table' }
  put fs { fs_cfg = cfg }
  return cfg

up_pc :: IntTState -> TId -> Pos -> IntTState
up_pc i@IntTState{..} t p =
  let st' = update_pc st t p
  in i {st = st'} 

handle_mark :: WItem -> FixOp (IntState,Pos,Act)
handle_mark (pre,eId,post) = do
  fs@FixState{..} <- get
  let node_st = case get_node_info fs_cfg pre of
        [s] -> fst s
        l   -> error $ "handle_mark invalid pre states: " ++ show l
      -- get the edge info
      e@EdgeInfo{..} = get_edge_info fs_cfg eId
      -- construct the transformer state
      tr_st = IntTState (Local fs_tid) node_st fs_symt fs_cfgs (is_cond edge_tags)
      -- decide based on the type of edge which transformer to call
      (acts,_ns) = case edge_code of
        -- execute the transformer
        D decl -> runState (transformer_decl decl) tr_st 
        E expr -> runState (transformer_expr expr) tr_st
      ns@IntTState{..} = up_pc _ns fs_tid post 
      (is_fix,node_table') =
         if is_join edge_tags
         -- join point: join the info in the cfg 
         then join_update (node_table fs_cfg) post (st,acts) 
         -- considering everything else the standard one: just replace 
         -- the information in the cfg and add the succs of post to the worklist
         else strong_update (node_table fs_cfg) post (st,acts) 
  cfg' <- update_node_table node_table'
  -- find the final result of the post
  case M.lookup post $ node_table cfg' of
    Just [(res_st,res_act)] -> return (res_st,post,res_act)
    _ -> error "handle_mark: unexcepted value in the final node_table"

fixpt_result :: FixOp ResultList
fixpt_result = do
  fs@FixState{..} <- get
  let marks = S.toList fs_mark
  res <- mapM handle_mark marks
  if fs_mode
  then do 
    fs@FixState{..} <- get
    return $ M.foldWithKey (\p l r -> map (\(a,b) -> (a,p,b)) l ++ r) [] $ node_table fs_cfg 
  else return $ res 

-- standard worklist algorithm
--  we have reached a fixpoint when the worklist is empty
worklist :: Worklist -> FixOp ResultList 
worklist _wlist = T.trace ("worklist: " ++ show _wlist) $ do
  fs@FixState{..} <- get
  case _wlist of
    [] -> fixpt_result 
    (it@(pre,eId,post):wlist) -> do
      -- get the current state in the pre
      let node_st = case get_node_info fs_cfg pre of
            [s] -> fst s
            l   -> error $ "worklist invalid pre states: " ++ show l
          -- get the edge info
          e@EdgeInfo{..} = get_edge_info fs_cfg eId
          -- construct the transformer state
          tr_st = IntTState (Local fs_tid) node_st fs_symt fs_cfgs (is_cond edge_tags)
          -- decide based on the type of edge which transformer to call
          (acts,ns@IntTState{..}) = case edge_code of
            -- execute the transformer
            D decl -> runState (transformer_decl decl) tr_st 
            E expr -> runState (transformer_expr expr) tr_st
      -- depending on whether the action is global or not;
      -- either add the results to the result list or update
      -- the cfg with them 
      if isGlobal acts || (Exit `elem` edge_tags)
      then T.trace ("is a global operation") $ do
        add_mark it
        worklist wlist
      else do 
        -- depending on the tags of the edge; the behaviour is different
        let (is_fix,node_table') =
             if is_join edge_tags
             -- join point: join the info in the cfg 
             then join_update (node_table fs_cfg) post (st,acts) 
             -- considering everything else the standard one: just replace 
             -- the information in the cfg and add the succs of post to the worklist
             else strong_update (node_table fs_cfg) post (st,acts) 
        cfg' <- update_node_table node_table'
        let rwlst = map (\(a,b) -> (post,a,b)) $ succs cfg' post
            nwlist = if is_fix then wlist else (wlist ++ rwlst)
        worklist nwlist

join_update :: NodeTable -> NodeId -> (IntState, Act) -> (Bool, NodeTable)
join_update node_table node (st,act) =
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [(st,act)] node_table)
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        let nst = st `join_intstate` st'
            nact = act `join_act` act'
        in if nst == st'
           then (True, node_table)
           else (False, M.insert node [(nst,nact)] node_table)
      _ -> error "join_update: more than one state in the list"
 
strong_update :: NodeTable -> NodeId -> (IntState,Act) -> (Bool, NodeTable)
strong_update node_table node (st,act) =
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [(st,act)] node_table) 
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        if st == st'
        then (True, node_table)
        else (False, M.insert node [(st,act)] node_table)
      _ -> error "strong_update: more than one state in the list" 
