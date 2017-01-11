{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Transformers.Util
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Utility operations for the transformers for the interval semantics.
-------------------------------------------------------------------------------
module Domain.Interval.Transformers.Util where

import Data.Map (Map)  
import Domain.Interval.State
import Domain.Interval.Value
import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Flow
import Util.Generic hiding (safeLookup)
import qualified Data.Map as M
import qualified Model.GCS as GCS

-- | retrieves the entry node of the cfg of a function
get_entry :: String -> Graphs SymId () (IntState, IntAct) -> Map SymId Symbol -> (GCS.Pos, SymId)
get_entry fnname graphs symt = 
  case M.foldrWithKey aux_get_entry Nothing graphs of
    Nothing -> error $ "get_entry: cant get entry for " ++ fnname
    Just p -> p -- T.trace (show symt) p
 where 
   aux_get_entry sym cfg r = 
     case r of 
       Just res -> Just res
       Nothing -> 
         case M.lookup sym symt of
           Nothing -> Nothing
           Just s  -> if get_name s == fnname
                      then Just (entry_node cfg, sym)
                      else Nothing 
  
-- | can_get_addrs_expr :: SExpression -> Bool
can_get_addrs_expr :: SExpression -> Bool
can_get_addrs_expr expr = case expr of
    Var id -> True 
    Unary CAdrOp e -> True 
    Index lhs rhs  -> True
    _ -> False

-- negates logical expression using De Morgan Laws
negExp :: SExpression -> SExpression
negExp expr = case expr of
  Binary CLndOp l r -> Binary CLorOp (negExp l) (negExp r)
  Binary CLorOp l r -> Binary CLndOp (negExp l) (negExp r)
  Binary op l r -> Binary (negOp op) l r
  Unary CNegOp e -> negExp e
  _ -> error $ "negExp: unsupported " ++ show expr

negOp :: BinaryOp -> BinaryOp 
negOp op = case op of
  CLeOp -> CGeqOp
  CGrOp -> CLeqOp
  CLeqOp -> CGrOp
  CGeqOp -> CLeOp
  CEqOp -> CNeqOp
  CNeqOp -> CEqOp
  _ -> error $ "negOp: unsupported " ++ show op
