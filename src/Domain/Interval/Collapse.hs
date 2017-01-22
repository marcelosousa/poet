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
import Domain.Interval.API (update_pc, read_memory)
import Domain.Interval.State
import Domain.Interval.Transformers.Declaration (transformer_decl)
import Domain.Interval.Transformers.Expression (transformer_expr)
import Domain.Interval.Transformers.State
import Domain.Interval.Transformers.Statement (get_addrs_expr, get_tid_expr, has_exited, is_locked)
import Domain.Interval.Value
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Converter (get_symbol_name)
import Language.SimpleC.Flow hiding (trace)
import Language.SimpleC.Util hiding (cfgs,symt)
import Model.GCS
import Util.Generic 
import qualified Data.Map as M
import qualified Data.Set as S

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
  -- The final worklist: every edge in this worklist is a global action 
  , fs_mark :: Set WItem 
  -- Map from loop heads to counter of traversals.  
  , fs_wide :: Map NodeId Int
  -- Widening level
  , fs_wid  :: Int 
  }

type FixOp val = State FixState val

get_widening_level :: FixOp Int
get_widening_level = do
  fs@FixState{..} <- get
  return fs_wid

inc_wide_node :: NodeId -> FixOp ()
inc_wide_node node_id = do
  fs@FixState{..} <- get
  let fs_wide' = case M.lookup node_id fs_wide of
        Nothing -> M.insert node_id 1 fs_wide
        Just n  -> M.insert node_id (n+1) fs_wide
  put fs { fs_wide = fs_wide' } 

get_wide_node :: NodeId -> FixOp Int
get_wide_node node_id = do
  fs@FixState{..} <- get
  case M.lookup node_id fs_wide of
    Nothing -> return 0
    Just n  -> return n 

showResultList :: ResultList -> String
showResultList l = "Data Flow Information:\n" ++ (snd $ foldr (\(s,p,a) (n,r) -> 
   let s_a = "CFG Node: " ++ show p ++ "\n"
       s_r = s_a ++ show s
   in (n+1, s_r ++ r))  (1,"") l)

-- | Instead of just looking at the immediate edge, one needs to potentially
--   traverse the graph until reaching a global action. Only at those leafs
--   one can compute the right result with respect to enabledness.
--   I will opt to not implement such procedure and generate events that are 
--   potentially only local.
is_live :: TId -> System IntState IntAct -> EdgeId -> IntGraph -> IntState -> Bool
is_live tid syst eId cfg st = 
  let EdgeInfo tags code = get_edge_info cfg eId 
  in case code of
    E (Call fname args _) -> case fname of
      Var ident -> case get_symbol_name ident (symt syst) of
        "pthread_join" ->
          let tid' = get_tid_expr (Local tid) st (args!!0) 
           -- not exited
          -- in not $ is_enabled syst st tid' 
          in has_exited (cfgs syst) st tid' 
         -- assume the mutex is declared globally 
        "pthread_mutex_lock" -> not $ is_locked st (Local tid) (args!!0)
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
  collapse b wid syst@System{..} st tid = 
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
    in mytrace False ("collapse: fixpoint of thread "++show tid ++ ", position = " ++ show pos) $
       let res = fixpt wid syst b tid cfgs symt th_cfg pos st
       in mytrace False "collapse: end" res

fixpt :: Int -> System IntState IntAct -> Bool -> TId -> IntGraphs -> SymbolTable -> IntGraph -> Pos -> IntState -> ResultList
fixpt wid syst b tid cfgs symt cfg@Graph{..} pos st =
  mytrace False ("fixpt: tid = " ++ show tid ++ " \n" ++ show st) $ 
  -- reset the node table information with the information passed
  let node_table' = M.insert pos [(st,bot_act)] $ M.map (const []) node_table
      cfg' = cfg { node_table = node_table' }
      wlist = map (\(a,b) -> (pos,a,b)) $ succs cfg' pos
      i_fix_st = FixState b tid cfgs symt cfg' S.empty M.empty wid 
      res = mytrace False ("fixpt: cfg = " ++ show ( node_table' :: NodeTable )) $ evalState (worklist syst wlist) i_fix_st
  in res 

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
handle_mark (pre,eId,post) = mytrace False ("handle_mark: " ++ show (pre,eId,post)) $ do
  fs@FixState{..} <- get
  let (node_st, pre_acts) = case get_node_info fs_cfg pre of
        [s] -> s
        l   -> error $ "handle_mark invalid pre states: " ++ show l
      -- get the edge info
      e@EdgeInfo{..} = mytrace False ("handle_mark: " ++ show node_st) $ get_edge_info fs_cfg eId
      -- construct the transformer state
      tr_st = IntTState (Local fs_tid) node_st fs_symt fs_cfgs (is_cond edge_tags) pre
      -- decide based on the type of edge which transformer to call
      (post_acts,_ns) = case edge_code of
        -- execute the transformer
        D decl -> runState (transformer_decl decl) tr_st 
        E expr -> runState (transformer_expr expr) tr_st
      ns@IntTState{..} = up_pc _ns fs_tid post 
      acts = pre_acts `join_act` post_acts
  (is_fix,node_table') <- case edge_tags of
    -- loop head point 
    [LoopHead] -> loop_head_update (node_table fs_cfg) post (st,acts)
    -- join point: join the info in the cfg 
    [IfJoin] -> return $ join_update (node_table fs_cfg) post (st,acts) 
    -- considering everything else the standard one: just replace 
    -- the information in the cfg and add the succs of post to the worklist
    _ -> return $ strong_update (node_table fs_cfg) post (st,acts) 
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
  res <- mytrace False ("fixpt_result: marks = " ++ show marks) $ mapM handle_mark marks
  if fs_mode
  then do 
    fs@FixState{..} <- get
    let table = node_table fs_cfg
        nodes = M.filterWithKey (\k _ -> not $ any (\(_,p,_) -> p == k) res) table
        nodes_res = M.foldWithKey (\p l r -> map (\(a,b) -> (a,p,b)) l ++ r) [] nodes 
    return $ res ++ nodes_res 
  else return $ res 

-- standard worklist algorithm
--  we have reached a fixpoint when the worklist is empty
worklist :: System IntState IntAct -> Worklist -> FixOp ResultList 
worklist syst _wlist = mytrace False ("worklist: " ++ show _wlist) $ do
  fs@FixState{..} <- get
  case _wlist of
    [] -> fixpt_result 
    (it@(pre,eId,post):wlist) -> do
      -- get the current state in the pre
      let (node_st, pre_acts) = case get_node_info fs_cfg pre of
            [s] -> s
            l   -> error $ "worklist invalid pre states: " ++ show l
          -- get the edge info
          e@EdgeInfo{..} = get_edge_info fs_cfg eId
          -- construct the transformer state
          tr_st = IntTState (Local fs_tid) node_st fs_symt fs_cfgs (is_cond edge_tags) pre
          -- decide based on the type of edge which transformer to call
          (post_acts,_ns) = case edge_code of
            -- execute the transformer
            D decl -> runState (transformer_decl decl) tr_st 
            E expr -> runState (transformer_expr expr) tr_st
          rwlst = map (\(a,b) -> (post,a,b)) $ succs fs_cfg post
          -- join the actions of the predecessors with the actions of the current edge
          acts = pre_acts `join_act` post_acts
          ns = up_pc _ns fs_tid post 
      -- depending on whether the action is global or not;
      -- either add the results to the result list or update
      -- the cfg with them 
      if is_bot (st ns)
      then mytrace False ("worklist: current edge returns bottom") $ worklist syst wlist
      else if isGlobal acts || (Exit `elem` edge_tags) || null rwlst 
           then mytrace False ("is a global operation") $ do
             add_mark it
             worklist syst wlist
           else mytrace False ("worklist: returned state\n" ++ show (st ns)) $ do 
             -- depending on the tags of the edge; the behaviour is different
             (is_fix,node_table') <-
                  case edge_tags of
                    -- loop head point 
                    [LoopHead] -> loop_head_update (node_table fs_cfg) post (st ns,acts)
                    -- join point: join the info in the cfg 
                    [IfJoin] -> return $ join_update (node_table fs_cfg) post (st ns,acts) 
                    -- considering everything else the standard one: just replace 
                    -- the information in the cfg and add the succs of post to the worklist
                    _ -> return $ strong_update (node_table fs_cfg) post (st ns,acts) 
             cfg' <- update_node_table node_table'
             let nwlist = if is_fix then wlist else (wlist ++ rwlst)
             worklist syst nwlist
            -- disabled_rwlst <- filterM (check_enabledness_succ syst) rwlst
            -- -- @NOTE: If one the sucessors is not enabled then
            -- -- simply mark it as a final node
            -- if not $ null disabled_rwlst
            -- then mytrace False ("worklist: non-global event!") $ do
            --   add_mark it
            --   worklist syst wlist
            -- else worklist syst nwlist

-- | Returns true if the current edge is disabled
check_enabledness_succ :: System IntState IntAct -> (NodeId, EdgeId, NodeId) -> FixOp Bool 
check_enabledness_succ syst (pre,eId,post) = do 
  fs@FixState{..} <- get
  let (node_st, pre_acts) = case get_node_info fs_cfg pre of
        [s] -> s
        l   -> error $ "check_enabledness_succ: invalid pre states = " ++ show l
  return $ not $ is_live fs_tid syst eId fs_cfg node_st

loop_head_update :: NodeTable -> NodeId -> (IntState, IntAct) -> FixOp (Bool, NodeTable)
loop_head_update node_table node (st,act) =  do
  c <- get_wide_node node
  inc_wide_node node
  w <- get_widening_level
  if c >= w 
  then mytrace False ("loop_head_update: going to apply widening") $ do 
    case M.lookup node node_table of
      -- error "loop_head_update: widening between a state and empty?" 
      Nothing ->  return $ (False, M.insert node [(st,act)] node_table)
      Just lst -> case lst of
        [] -> error "loop_head_update: widening between a state and empty?" 
        [(st',act')] ->
          let nst = st' `widen_intstate` st
              nact = act `join_act` act'
          in if nst == st'
             then return $ (True, node_table)
             else return $ (False, M.insert node [(nst,nact)] node_table)
        _ -> error "loop_head_update: widening between a state and several states?" 
  else do
    mytrace False ("loop_head_update: not going to apply widening " ++ show c) $ return $ join_update node_table node (st,act) 

join_update :: NodeTable -> NodeId -> (IntState, IntAct) -> (Bool, NodeTable)
join_update node_table node (st,act) =
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [(st,act)] node_table)
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        let nst = st `join_intstate` st'
            nact = act `join_act` act'
        in mytrace False ("join_update: old state:\n" ++ show st' ++ "join_update: new state\n" ++ show st ++ "join_update:join state\n" ++ show nst) $ if nst == st'
           then (True, node_table)
           else (False, M.insert node [(nst,nact)] node_table)
      _ -> error "join_update: more than one state in the list"
 
strong_update :: NodeTable -> NodeId -> (IntState,IntAct) -> (Bool, NodeTable)
strong_update node_table node (st,act) = mytrace False ("strong_update: node = " ++ show node ++ ", state\n" ++ show st) $
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [(st,act)] node_table) 
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        if st == st'
        then (True, node_table)
        else (False, M.insert node [(st,act `join_act` act')] node_table)
      _ -> error "strong_update: more than one state in the list" 
