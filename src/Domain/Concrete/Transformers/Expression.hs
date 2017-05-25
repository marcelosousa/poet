{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Transformers.Expression
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- Transformers for the concrete semantics.
-- Two main transformers for the types of edges in the CFG:
--   transformer_expr (for the expression) 
-------------------------------------------------------------------------------
module Domain.Concrete.Transformers.Expression (transformer_expr) where

import Control.Monad.State.Lazy 
import Data.List 
import Data.Map (Map)  
import Data.Maybe
import Domain.Action
import Domain.Concrete.API
import Domain.Concrete.State
import Domain.Concrete.Transformers.State
import Domain.Concrete.Transformers.Statement
import Domain.Concrete.Value
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

-- Given an initial state and an expression
-- return the updated state.
transformer_expr :: SExpression -> ConTOp ConAct
transformer_expr expr = mytrace False ("transformer_expr: " ++ show expr) $ do
  s@ConTState{..} <- get
  if cond
  then mytrace False ("transformer_expr: conditional " ++ show expr) $ do 
    (val, act) <- bool_transformer_expr expr
    s@ConTState{..} <- get
    let res_st = case val of
          ConBot -> set_cstate_bot st
          _ -> st
    set_state res_st 
    mytrace False ("bool_transformer: result = " ++ show val) $ return act
  else do
    (vals,act) <- transformer expr
    return act 
    
-- eval logical expressions
bool_transformer_expr :: SExpression -> ConTOp (ConValue, ConAct)
bool_transformer_expr expr = 
 case expr of
  Binary op lhs rhs -> apply_logic op lhs rhs
  Unary CNegOp rhs ->
    let rhs' = negExp rhs
    in bool_transformer_expr rhs' 
  _ -> error $ "bool_transformer_expr: not supported " ++ show expr

apply_logic :: BinaryOp -> SExpression -> SExpression -> ConTOp (ConValue,ConAct)
apply_logic op lhs rhs = do
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_acts = lhs_acts `join_act` rhs_acts 
      res_val = case op of
        CLeOp  -> le_conval   lhs_vals rhs_vals 
        CGrOp  -> gr_conval   lhs_vals rhs_vals 
        CLeqOp -> leq_conval  lhs_vals rhs_vals 
        CGeqOp -> geq_conval  lhs_vals rhs_vals 
        CEqOp  -> eq_conval   lhs_vals rhs_vals 
        CNeqOp -> neq_conval  lhs_vals rhs_vals 
        CLndOp -> land_conval lhs_vals rhs_vals 
        CLorOp -> lor_conval  lhs_vals rhs_vals 
  return (res_val,res_acts)

