{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Transformers.Expression
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Transformers for the interval semantics.
-- Two main transformers for the types of edges in the CFG:
--   transformer_expr (for the expression) 
-------------------------------------------------------------------------------
module Domain.Interval.Transformers.Expression (transformer_expr) where

import Control.Monad.State.Lazy 
import Data.List 
import Data.Map (Map)  
import Data.Maybe
import Domain.Action
import Domain.Interval.API
import Domain.Interval.State
import Domain.Interval.Transformers.State
import Domain.Interval.Transformers.Statement
import Domain.Interval.Transformers.Util
import Domain.Interval.Type
import Domain.Interval.Value
import Domain.MemAddr
import Domain.Util
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Flow
import Language.SimpleC.Util
import Util.Generic hiding (safeLookup)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Model.GCS as GCS

-- | Transformers for interval semantics 
-- Given an initial state and an expression
-- return the updated state.
transformer_expr :: SExpression -> IntTOp IntAct
transformer_expr expr = mytrace False ("transformer_expr: " ++ show expr) $ do
  s@IntTState{..} <- get
  if cond
  then mytrace False ("transformer_expr: conditional " ++ show expr) $ do 
    (val, act) <- bool_transformer_expr expr
    s@IntTState{..} <- get
    let res_st = case val of
          IntBot -> set_int_state_bot st
          _ -> st
    set_state res_st 
    mytrace False ("bool_transformer: result = " ++ show val) $ return act
  else do
    (vals,act) <- transformer expr
    return act 
    
-- eval logical expressions
-- we are going to be conservative;
-- all the variables in this expression
-- are going to be considered written.
-- We follow the implementation described
-- in https://www.di.ens.fr/~rival/semverif-2016/sem-11-ai.pdf
-- starting from page 28.
-- Only <=, \/ and /\ will be implemented.
bool_transformer_expr :: SExpression -> IntTOp (IntValue, IntAct)
bool_transformer_expr expr = case expr of
  Binary op lhs rhs -> apply_logic op lhs rhs
  Unary CNegOp rhs ->
    let rhs' = negExp rhs
    in bool_transformer_expr rhs' 
  _ -> error $ "bool_transformer_expr: not supported " ++ show expr

apply_logic :: BinaryOp -> SExpression -> SExpression -> IntTOp (IntValue,IntAct)
apply_logic op lhs rhs =
  let one = Const $ IntConst $ CInteger 1 DecRepr $ Flags 0 
  in case op of
    -- e1 < e2 ~> e1 <= (e2 - 1)
    CLeOp  -> interval_leq lhs (Binary CSubOp rhs one)
    -- e1 > e2 ~> e2 <= (e1 - 1)
    CGrOp  -> interval_leq (Binary CAddOp rhs one) lhs
    -- e1 <= e2
    CLeqOp -> interval_leq lhs rhs
    -- e1 >= e2 ~> e2 <= e1
    CGeqOp -> interval_leq rhs lhs
    -- e1 == e2 ~> (e1 <= e2) and (e2 <= e1)
    CEqOp  ->
      let lhs' = Binary CLeqOp lhs rhs
          rhs' = Binary CLeqOp rhs lhs
      in apply_logic CLndOp lhs' rhs'
    -- e1 != e2 ~> (e1 <= (e2 - 1)) or (e2 <= (e1 - 1))
    CNeqOp ->
      let lhs' = Binary CLeqOp lhs (Binary CSubOp rhs one)
          rhs' = Binary CLeqOp rhs (Binary CSubOp lhs one)
      in apply_logic CLorOp lhs' rhs'
    CLndOp -> do
      (lhs_val, lhs_act) <- bool_transformer_expr lhs 
      (rhs_val, rhs_act) <- bool_transformer_expr rhs
      let acts = lhs_act `join_act` rhs_act
      if lhs_val /= IntBot && rhs_val /= IntBot
      then return (lhs_val, acts)
      else return (IntBot, acts)
    CLorOp -> do
      (lhs_val, lhs_act) <- bool_transformer_expr lhs 
      (rhs_val, rhs_act) <- bool_transformer_expr rhs
      let res_val = if lhs_val /= IntBot 
                    then lhs_val
                    else if rhs_val /= IntBot
                         then rhs_val
                         else IntBot
      return (res_val, lhs_act `join_act` rhs_act)

-- Logical Operations
-- Need to update the variables
interval_leq :: SExpression -> SExpression -> IntTOp (IntValue, IntAct)
interval_leq lhs rhs = mytrace False ("inter_leq: lhs = " ++ show lhs ++ ", rhs = " ++ show rhs) $ do
  (lhs_val, lhs_act) <- transformer lhs 
  (rhs_val, rhs_act) <- transformer rhs 
  let acts = lhs_act `join_act` rhs_act
      (a,b) = (lowerBound lhs_val, upperBound lhs_val)
      (c,d) = (lowerBound rhs_val, upperBound rhs_val)
  if lhs_val == IntBot || rhs_val == IntBot || a > d
  then return (IntBot, acts)
  else do
    s@IntTState{..} <- get
    -- Update the variables if we can 
    let lhs_nval = InterVal (a, min b d)
        rhs_nval = InterVal (max a c, d) 
        (lhs_st, lhs_nact) = 
          if can_get_addrs_expr lhs 
          then let lhs_addr = get_addrs_expr st scope lhs
               in (write_memory st lhs_addr lhs_nval, write_act_addr lhs_addr)
          else (st, bot_act)
        (rhs_st, rhs_nact) = 
          if can_get_addrs_expr rhs 
          then let rhs_addr = get_addrs_expr st scope rhs
               in (write_memory lhs_st rhs_addr rhs_nval, write_act_addr rhs_addr)
          else (lhs_st, bot_act)
        final_acts = acts `join_act` lhs_nact `join_act` rhs_nact
    set_state rhs_st
    trace ("interval_leq: lhs_val = " ++ show lhs_val ++ ", rhs_val = " ++ show rhs_val ++ ", lhs_nval = " ++ show lhs_nval ++ ", rhs_nval = " ++ show rhs_nval) $ return (lhs_nval, final_acts)
