{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Converter
-- Copyright :  (c) 2015-16 Marcelo Sousa
--
-- Transformers for the concrete semantics.
-- Two main transformers for the types of edges in the CFG:
--   transformer_decl (for the declaration)
--   transformer_expr (for the expression) 
-------------------------------------------------------------------------------
module Domain.Concrete.Converter where

import Control.Monad.State.Lazy 

import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)  
import Debug.Trace
import Data.List 
import qualified Data.Set as S

import Domain.Concrete.State
import Domain.Action
import Domain.Util

import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Flow
import Language.SimpleC.Util
import qualified Model.GCS as GCS
import Util.Generic hiding (safeLookup)
import qualified Debug.Trace as T


-- | Transformer for binary operations.
--   These transformers do not change the state;
binop_transformer :: BinaryOp -> SExpression -> SExpression -> ConTOp (ConValues,Act)
binop_transformer binOp lhs rhs = do
  s@ConTState{..} <- get
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_vals = case binOp of
        CMulOp -> lhs_vals `mult` rhs_vals 
        CDivOp -> lhs_vals `divs` rhs_vals 
        CRmdOp -> lhs_vals `rmdr` rhs_vals 
        CAddOp -> lhs_vals `add` rhs_vals 
        CSubOp -> lhs_vals `sub` rhs_vals
        -- boolean operations 
        CLeOp  -> lhs_vals `le` rhs_vals 
        CGrOp  -> lhs_vals `gr` rhs_vals 
        CLeqOp -> lhs_vals `leq` rhs_vals  
        CGeqOp -> lhs_vals `geq` rhs_vals 
        CEqOp  -> lhs_vals `eq` rhs_vals 
        CNeqOp -> lhs_vals `neq` rhs_vals 
        CAndOp -> lhs_vals `land` rhs_vals 
        COrOp  -> lhs_vals `lor` rhs_vals
        -- bit-wise operations 
        CShlOp -> error "binop_transformer: CShlOp not supported"  
        CShrOp -> error "binop_transformer: CShrOp not supported" 
        CXorOp -> error "binop_transformer: CXorOp not supported" 
        CLndOp -> error "binop_transformer: CLndOp not supported" 
        CLorOp -> error "binop_transformer: CLorOp not supported"
      res_acts = lhs_acts `join_act` rhs_acts
  return (res_vals,res_acts)


cond_transformer :: SExpression -> Maybe SExpression -> SExpression -> ConTOp (ConValues,Act)
cond_transformer cond mThen else_e = do
  (vals,acts) <- transformer cond
  -- vals is both false and true 
  let (isTrue,isFalse) = checkBoolVals vals
  (tVal,tAct) <-
    if isTrue
    then case mThen of
           Nothing -> error "cond_transformer: cond is true and there is not then"
           Just e -> transformer e
    else return ([],bot_act)
  (eVal,eAct) <-
    if isFalse
    then transformer else_e
    else return ([],bot_act)
  let res_vals = tVal ++ eVal
      res_acts = acts `join_act` tAct `join_act` eAct
  return (res_vals,res_acts) 




-- Binary transformers
-- Arithmetic transformers 
-- mult,divs, :: ConValues -> ConValues -> ConValues
gen_op op v1 v2 = [e1 `op` e2 | e1 <- v1, e2 <- v2]
add  = gen_op add_conval 
sub  = gen_op sub_conval  
mult = gen_op mult_conval 
divs = gen_op divs_conval
rmdr = gen_op rmdr_conval 
-- Boolean operations
le   = gen_op le_conval 
gr   = gen_op gr_conval 
leq  = gen_op leq_conval 
geq  = gen_op geq_conval 
eq   = gen_op eq_conval 
neq  = gen_op neq_conval 
land = gen_op land_conval 
lor  = gen_op lor_conval 

-- Unary operations
minus, neg_tr :: ConValues -> ConValues
minus v1 = [ minus_conval v | v <- v1 ] 
neg_tr v1 = [ neg_conval v | v <- v1 ] 
