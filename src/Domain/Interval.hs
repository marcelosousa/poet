{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval
-- Copyright :  (c) 2015-16 Marcelo Sousa
-- 
-- Modules for the interval semantics
-------------------------------------------------------------------------------
module Domain.Interval () where

import Control.Monad.State.Lazy hiding (join)
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Domain.Action
import Domain.Class
import Domain.Interval.API (update_pc, read_memory)
import Domain.Interval.State
import Domain.Interval.Transformers.Declaration (transformer_decl)
import Domain.Interval.Transformers.Expression (transformer_expr)
import Domain.Interval.Transformers.State
import Domain.Interval.Transformers.Statement (get_addrs_expr, get_tid_expr, has_exited, is_locked)
import Domain.Interval.Value
import Domain.Lattice
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Converter (get_symbol_name)
import Language.SimpleC.Flow hiding (trace)
import Language.SimpleC.Util hiding (cfgs,symt)
import Model.GCS
import Util.Generic 
import qualified Data.Map as M
import qualified Data.Set as S

type IntGraph      = CGraph     IntState IntAct
type IntGraphs     = CGraphs    IntState IntAct
type IntResultList = ResultList IntState IntAct
type IntNodeTable  = NodeTable  IntState IntAct
type IntFixOp val  = FixOp      IntState IntAct val

instance Domain IntState IntAct where
  is_enabled       = is_enabled_int
  code_transformer = code_transformer_int
  weak_update      = weak_update_int
  strong_update    = strong_update_int
  loop_head_update = loop_head_update_int   
  run              = run_int

-- Enabledness transformer for Interval State
is_enabled_int :: System IntState IntAct -> IntState -> TId -> Bool
is_enabled_int syst st tid =
  let control     = controlPart st
      tid_cfg_sym = toThCFGSym st tid
  in case M.lookup tid control of
       Nothing  -> False
       Just pos -> case M.lookup tid_cfg_sym (cfgs syst) of 
         Nothing  -> error $ "is_enabled fatal: tid " ++ show tid ++ " not found in cfgs"
         Just cfg -> case succs cfg pos of
           [] -> False
           s  -> all (\(eId,nId) -> is_live tid syst eId cfg st) s

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

-- | Calls the appropriated transformer depending on the type of edge
code_transformer_int :: NodeId -> NodeId -> EdgeInfo SymId () -> IntState -> IntFixOp (IntAct, IntState, Set Int)
code_transformer_int pre post e@EdgeInfo{..} node_st = do
  fs@FixState{..} <- get      
  let is_c = is_cond edge_tags
      -- construct the transformer state
      tr_st = IntTState (Local fs_tid) node_st fs_symt fs_cfgs is_c pre fs_warns
      -- decide based on the type of edge which transformer to call
      (post_acts,n_tr_st) = case edge_code of
        -- execute the transformer
        D decl -> runState (transformer_decl decl) tr_st 
        E expr -> runState (transformer_expr expr) tr_st
      state  = st n_tr_st
      state' = update_pc state fs_tid post
  return (post_acts,state',warns n_tr_st)

-- | Loop head update
loop_head_update_int :: IntNodeTable -> NodeId -> (IntState, IntAct) -> IntFixOp (Bool, IntNodeTable)
loop_head_update_int node_table node (st,act) =  do
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
          let nst  = st' `widen` st
              nact = act `join` act'
          in if nst == st'
             then return $ (True, node_table)
             else return $ (False, M.insert node [(nst,nact)] node_table)
        _ -> error "loop_head_update: widening between a state and several states?" 
  else weak_update node_table node (st,act) 

-- | Weak update
weak_update_int :: IntNodeTable -> NodeId -> (IntState, IntAct) -> IntFixOp (Bool, IntNodeTable)
weak_update_int node_table node (st,act) = 
  case M.lookup node node_table of
    Nothing  -> return (False, M.insert node [(st,act)] node_table)
    Just lst -> case lst of
      []           -> return (False, M.insert node [(st,act)] node_table)
      [(st',act')] -> do
        let nst  = st  `join` st'
            nact = act `join` act'
        if nst == st'
        then return (True, node_table)
        else return (False, M.insert node [(nst,nact)] node_table)
      _ -> error "join_update: more than one state in the list"
 
-- | Strong update
strong_update_int :: IntNodeTable -> NodeId -> (IntState,IntAct) -> IntFixOp (Bool, IntNodeTable)
strong_update_int node_table node (st,act) =
  case M.lookup node node_table of
    Nothing  -> return (False, M.insert node [(st,act)] node_table) 
    Just lst -> case lst of
      []           -> return (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        if st == st'
        then return (True, node_table)
        else return (False, M.insert node [(st,act `join` act')] node_table)
      _ -> error "strong_update: more than one state in the list" 


-- | Run the fixpoint computation
run_int :: Bool -> Int -> System IntState IntAct -> IntState -> TId -> (Set Int,ResultList IntState IntAct)
run_int b wid syst@System{..} st tid = 
    let control = controlPart st
        pos = case M.lookup tid control of
          Nothing -> error $ "run: tid " ++ show tid ++ " is not control"
          Just p  -> p
        th_cfg_sym = case M.lookup tid (th_states st) of
          Nothing -> error $ "run: cant find thread in the state " ++ show tid
          Just th_st -> th_cfg_id th_st 
        th_cfg = case M.lookup th_cfg_sym cfgs of
          Nothing -> error $ "run: cant find thread " ++ show th_cfg_sym
          Just cfg -> cfg 
    in mytrace False ("run: fixpoint of thread "++show tid ++ ", position = " ++ show pos) $
       let res = fixpt wid syst b tid cfgs symt th_cfg pos st
       in mytrace False "run: end" res