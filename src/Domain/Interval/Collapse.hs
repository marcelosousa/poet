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
  -- The final worklist: every edge in this worklist is a global action 
  , fs_mark :: Set WItem 
  -- Map from loop heads to counter of traversals.  
  , fs_wide :: Map NodeId Int
  }

type FixOp val = State FixState val

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

is_locked :: IntState -> Scope -> SExpression -> Bool
is_locked st scope expr = 
  let lk_addrs = get_addrs_just st scope expr 
  in case read_memory st lk_addrs of
    []    -> error $ "is_locked fatal: cant find info for lock " ++ show expr
    [val] -> val == one 
    l -> error $ "is_locked fatal: lock has unsupported values " ++ show l 

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
          in has_exited syst st tid' 
         -- assume the mutex is declared globally 
        "pthread_mutex_lock" -> not $ is_locked st Global (args!!0)
        _ -> True 
      _ -> True
    _ -> True

has_exited syst st tid = 
  let control = controlPart st
      tid_cfg_sym = toThCFGSym st tid
  in case M.lookup tid control of
       Nothing  -> False
       Just pos -> case M.lookup tid_cfg_sym (cfgs syst) of 
         Nothing  -> error $ "has_exited fatal: tid " ++ show tid ++ " not found in cfgs"
         Just cfg -> case succs cfg pos of
           [] -> True
           _ -> False 
 
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
       let res = fixpt syst b tid cfgs symt th_cfg pos st
       in res `seq` (mtrace "collapse: end" $ res)

-- @TODO: Receive other options for widening and state splitting
fixpt :: System IntState IntAct -> Bool -> TId -> IntGraphs -> SymbolTable -> IntGraph -> Pos -> IntState -> ResultList 
fixpt syst b tid cfgs symt cfg@Graph{..} pos st =
  -- reset the node table information with the information passed
  let node_table' = M.insert pos [(st,bot_act)] $ M.map (const []) node_table
      cfg' = cfg { node_table = node_table' }
      wlist = map (\(a,b) -> (pos,a,b)) $ succs cfg' pos
      i_fix_st = FixState b tid cfgs symt cfg' S.empty M.empty
  in  evalState (worklist syst wlist) i_fix_st 

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
  let (node_st, pre_acts) = case get_node_info fs_cfg pre of
        [s] -> s
        l   -> error $ "handle_mark invalid pre states: " ++ show l
      -- get the edge info
      e@EdgeInfo{..} = get_edge_info fs_cfg eId
      -- construct the transformer state
      tr_st = IntTState (Local fs_tid) node_st fs_symt fs_cfgs (is_cond edge_tags)
      -- decide based on the type of edge which transformer to call
      (post_acts,_ns) = trace ("handle_mark: state = " ++ show node_st) $ case edge_code of
        -- execute the transformer
        D decl -> runState (transformer_decl decl) tr_st 
        E expr -> runState (transformer_expr expr) tr_st
      acts = pre_acts `join_act` post_acts
      ns@IntTState{..} = up_pc _ns fs_tid post 
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
  res <- trace ("fixpt_result: marks = " ++ show marks) $ mapM handle_mark marks
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
worklist syst _wlist = T.trace ("worklist: " ++ show _wlist) $ do
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
          tr_st = IntTState (Local fs_tid) node_st fs_symt fs_cfgs (is_cond edge_tags)
          -- decide based on the type of edge which transformer to call
          (post_acts,ns@IntTState{..}) = case edge_code of
            -- execute the transformer
            D decl -> runState (transformer_decl decl) tr_st 
            E expr -> runState (transformer_expr expr) tr_st
          rwlst = map (\(a,b) -> (post,a,b)) $ succs fs_cfg post
          acts = pre_acts `join_act` post_acts
      -- depending on whether the action is global or not;
      -- either add the results to the result list or update
      -- the cfg with them 
      if is_bot st
      then T.trace ("worklist: current edge returns bottom") $ worklist syst wlist
      else if isGlobal acts || (Exit `elem` edge_tags) || null rwlst 
           then mtrace ("is a global operation") $ do
             add_mark it
             worklist syst wlist
           else do 
             -- depending on the tags of the edge; the behaviour is different
             (is_fix,node_table') <-
                  case edge_tags of
                    -- loop head point 
                    [LoopHead] -> loop_head_update (node_table fs_cfg) post (st,acts)
                    -- join point: join the info in the cfg 
                    [IfJoin] -> return $ join_update (node_table fs_cfg) post (st,acts) 
                    -- considering everything else the standard one: just replace 
                    -- the information in the cfg and add the succs of post to the worklist
                    _ -> return $ strong_update (node_table fs_cfg) post (st,acts) 
             cfg' <- update_node_table node_table'
             let nwlist = if is_fix then wlist else (wlist ++ rwlst)
             disabled_rwlst <- filterM (check_enabledness_succ syst) rwlst
             -- @NOTE: If one the sucessors is not enabled then
             -- simply mark it as a final node
             if not $ null disabled_rwlst
             then T.trace ("worklist: non-global event!") $ do
               add_mark it
               worklist syst wlist
             else worklist syst nwlist

-- | Returns true if the current edge is disabled
check_enabledness_succ :: System IntState IntAct -> (NodeId, EdgeId, NodeId) -> FixOp Bool 
check_enabledness_succ syst (pre,eId,post) = do 
  fs@FixState{..} <- get
  let (node_st, pre_acts) = case get_node_info fs_cfg pre of
        [s] -> s
        l   -> error $ "check_enabledness_succ: invalid pre states = " ++ show l
  return $ not $ is_live fs_tid syst eId fs_cfg node_st

loop_head_update :: NodeTable -> NodeId -> (IntState, IntAct) -> FixOp (Bool, NodeTable)
loop_head_update node_table node (st,act) = T.trace ("loop_head_update: BEGIN") $ do
  c <- get_wide_node node
  inc_wide_node node
  if c >= 3
  then T.trace ("loop_head_update: going to apply widening") $ do 
    case M.lookup node node_table of
      Nothing -> error "loop_head_update: widening between a state and empty?" 
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
    return $ join_update node_table node (st,act) 

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
  trace ("strong_update: node = " ++ show node ++ ", acts = " ++ show act ++ ", state = " ++ show st) $ 
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [(st,act)] node_table) 
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        if st == st'
        then (True, node_table)
        else (False, M.insert node [(st,act `join_act` act')] node_table)
      _ -> error "strong_update: more than one state in the list" 
