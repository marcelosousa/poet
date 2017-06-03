{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- Collapse for concrete semantics:
--  This will amount to execute the current edge in the CFG
--  Optimize to execute all local executions 
-------------------------------------------------------------------------------
module Domain.Concrete () where

import Control.Monad.State.Lazy
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Domain.Action
import Domain.Class
import Domain.Concrete.API (update_pc, read_memory)
import Domain.Concrete.State
import Domain.Concrete.Transformers.Declaration (transformer_decl)
import Domain.Concrete.Transformers.Expression (transformer_expr)
import Domain.Concrete.Transformers.State
import Domain.Concrete.Transformers.Statement (get_addrs_expr, get_tid_expr, has_exited, is_locked)
import Domain.Concrete.Value
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Converter (get_symbol_name)
import Language.SimpleC.Flow hiding (trace)
import Language.SimpleC.Util hiding (cfgs,symt)
import Model.GCS
import Util.Generic 
import qualified Data.Map as M
import qualified Data.Set as S

type ConGraph      = CGraph     ConState ConAct
type ConGraphs     = CGraphs    ConState ConAct
type ConResultList = ResultList ConState ConAct
type ConNodeTable  = NodeTable  ConState ConAct
type ConFixOp val  = FixOp      ConState ConAct val

instance Domain ConState ConAct where
  is_enabled       = is_enabled_con
  is_live          = is_live_con
  code_transformer = code_transformer_con
  weak_update      = strong_update
  loop_head_update = strong_update
  run              = run_con
  
-- Enabledness transformer for Concrete State
is_enabled_con :: System ConState ConAct -> ConState -> TId -> Maybe ConState 
is_enabled_con syst st tid =
  let (warns,res) = run False 10000 syst st tid
  in case res of
       []  -> Nothing
       [(s,_,_)] -> Just s
       _   -> error "is_enabled_con: more than one state returned" 
{-
  -- OLD VERSION THAT RELIES ON is_enabled
  let control     = controlPart st
      tid_cfg_sym = toThCFGSym st tid
  in case M.lookup tid control of
       Nothing  -> False
       Just pos -> case M.lookup tid_cfg_sym (cfgs syst) of 
         Nothing  -> error $ "is_enabled: tid " ++ show tid ++ " not found in cfgs"
         Just cfg -> case succs cfg pos of
           [] -> False
           s  -> any (\(eId,nId) -> is_live_con tid syst eId nId cfg st) s
-}

-- | Instead of just looking at the immediate edge, one needs to potentially
--   traverse the graph until reaching a global action. Only at those leafs
--   one can compute the right result with respect to enabledness.
--   Thus, we will call the execution engine and memoise that call;
--   @TODO: Provide more information for future reference
is_live_con :: TId -> System ConState ConAct -> EdgeId -> NodeId -> ConGraph -> ConState -> Bool
is_live_con tid syst eId nId cfg st = 
  let EdgeInfo tags code = get_edge_info cfg eId 
  in case code of
    E (Call fname args _) -> case fname of
      Var ident -> case get_symbol_name ident (symt syst) of
        "pthread_create" -> True
        "pthread_join"   -> mytrace False ("is_live_con: " ++ show code) $
          let tid' = get_tid_expr (Local tid) st (args!!0) 
           -- not exited
          -- in not $ is_enabled syst st tid' 
          in has_exited (cfgs syst) st tid' 
         -- assume the mutex is declared globally 
        "pthread_mutex_lock"   -> not $ is_locked st (Local tid) (args!!0)
        "pthread_mutex_unlock" -> True
        _ -> is_live_con_aux 
      _ -> is_live_con_aux
    _ -> is_live_con_aux
  where 
    is_live_con_aux =
      case succs cfg nId of
        [] -> True
        s  -> any (\(eId',nId') -> is_live_con tid syst eId' nId' cfg st) s

-- | Calls the appropriated transformer depending on the type of edge
code_transformer_con :: NodeId -> NodeId -> EdgeInfo SymId () -> ConState -> ConFixOp (ConAct, ConState, Set Int)
code_transformer_con pre post e@EdgeInfo{..} node_st = mytrace False ("transformer: " ++ show edge_code) $ do
  fs@FixState{..} <- get      
  let is_c = is_cond edge_tags
      -- construct the transformer state
      tr_st = ConTState (Local fs_tid) node_st fs_symt fs_cfgs is_c False pre fs_warns
      -- decide based on the type of edge which transformer to call
      (post_acts,n_tr_st) = case edge_code of
        -- execute the transformer
        D decl -> runState (transformer_decl decl) tr_st 
        E expr -> runState (transformer_expr expr) tr_st
      state  = st n_tr_st
      state' = update_pc state fs_tid post
  return (post_acts,state',warns n_tr_st)

-- | Run the fixpoint computation
run_con :: Bool -> Int -> System ConState ConAct -> ConState -> TId -> (Set Int,ConResultList)
run_con b wid syst@System{..} st tid = mytrace True "run_con" $ 
  let control = controlPart st
      pos     = case M.lookup tid control of
        Nothing -> error $ "run_con: tid " ++ show tid ++ " not control"
        Just p  -> p
      th_cfg_sym = case M.lookup tid (cs_tstates st) of
        Nothing    -> error $ "run_con: cant find thread in " ++ show tid
        Just th_st -> th_cfg_id th_st 
      th_cfg = case M.lookup th_cfg_sym cfgs of
        Nothing  -> error $ "run_con: cant find thread " ++ show th_cfg_sym
        Just cfg -> cfg 
  in fixpt wid syst b tid cfgs symt th_cfg pos st
