{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Collapse
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Collapse for interval semantics 
-------------------------------------------------------------------------------
module Domain.Interval.Collapse where

import Control.Monad.State.Lazy
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Domain.Action
import Domain.Interval.Converter
import Domain.Interval.State
import Domain.Interval.Value
import Domain.Interval.API
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Converter (get_symbol_name)
import Language.SimpleC.Flow
import Language.SimpleC.Util hiding (cfgs,symt)
import Model.GCS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Debug.Trace as T

type IntGraph = Graph SymId () (IntState,IntAct) 
type IntGraphs = Graphs SymId () (IntState,IntAct)
type WItem = (NodeId,EdgeId,NodeId)
type Worklist = [WItem]
type ResultList = [(IntState,Pos,IntAct)]
type NodeTable = Map NodeId [(IntState,IntAct)]

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

is_locked :: IntState -> Scope -> SExpression -> Bool
is_locked st scope expr = 
  let lk_addrs = get_addrs st scope expr 
  in case read_memory st lk_addrs of
    []    -> error $ "is_locked fatal: cant find info for lock " ++ show expr
    [val] -> case val of 
      IntVal [VInt i] -> i == 1  
      _ -> error $ "is_locked fatal: lock has unsupported values " ++ show val 
    l -> error $ "is_locked fatal: lock has unsupported values " ++ show l 

is_live :: TId -> System IntState IntAct -> EdgeId -> IntGraph -> IntState -> Bool
is_live tid syst eId cfg st = 
  let EdgeInfo tags code = get_edge_info cfg eId 
  in case code of
    E (Call fname args _) -> case fname of
      Var ident -> case get_symbol_name ident (symt syst) of
        "pthread_join" ->
          let tid' = get_tid_expr (Local tid) st (args!!0) 
          in not $ is_enabled syst st tid' 
         -- assume the mutex is declared globally 
        "pthread_mutex_lock" -> not $ is_locked st Global (args!!0)
        _ -> True 
      _ -> True
    _ -> True
 
instance Collapsible IntState IntAct where
  is_enabled syst st tid =
    let control = controlPart st
        tid_cfg_sym = toThCFGSym st tid
    in case M.lookup tid control of
         Nothing  -> False
         Just pos -> case M.lookup tid_cfg_sym (cfgs syst) of 
           Nothing  -> error $ "is_enabled fatal: tid " ++ show tid ++ " not found in cfgs"
           Just cfg -> case succs cfg pos of
             [] -> False
             s  -> all (\(eId,nId) -> is_live tid syst eId cfg st) s
  collapse b syst@System{..} st tid = 
    let control = controlPart st
        pos = case M.lookup tid control of
          Nothing -> error $ "collapse: tid " ++ show tid ++ " is not control"
          Just p  -> p
        th_cfg_sym = case M.lookup tid (th_states st) of
          Nothing -> error $ "collapse: cant find thread in the state " ++ show tid
          Just th_st -> th_cfg_id th_st 
        th_cfg = case M.lookup th_cfg_sym cfgs of
          Nothing -> error $ "collapse: cant find thread " ++ show th_cfg_sym
          Just cfg -> cfg 
    in T.trace ("collapse: fixpoint of thread " ++ show tid ++ ", position = " ++ show pos) $ 
       let res = fixpt b tid cfgs symt th_cfg pos st
       in res `seq` (mtrace "collapse: end" $ res)

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

handle_mark :: WItem -> FixOp (IntState,Pos,IntAct)
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
    Just [(res_st,res_act)] -> do
      let rwlst = map (\(a,b) -> (post,a,b)) $ succs fs_cfg post
          e_act = exit_thread_act (SymId fs_tid) zero
      if (Exit `elem` edge_tags) || null rwlst
      then return (res_st,post,res_act `join_act` e_act)
      else return (res_st,post,res_act)
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
worklist _wlist = mtrace ("worklist: " ++ show _wlist) $ do
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
          rwlst = map (\(a,b) -> (post,a,b)) $ succs fs_cfg post
      -- depending on whether the action is global or not;
      -- either add the results to the result list or update
      -- the cfg with them 
      if isGlobal acts || (Exit `elem` edge_tags) || null rwlst
      then mtrace ("is a global operation") $ do
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
        let nwlist = if is_fix then wlist else (wlist ++ rwlst)
        worklist nwlist

join_update :: NodeTable -> NodeId -> (IntState, IntAct) -> (Bool, NodeTable)
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
 
strong_update :: NodeTable -> NodeId -> (IntState,IntAct) -> (Bool, NodeTable)
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
