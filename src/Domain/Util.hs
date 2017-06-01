{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Util
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Utility functions for abstract domains 
-------------------------------------------------------------------------------
module Domain.Util where

import Data.Hashable
import Data.Map (Map)
import Language.SimpleC.AST
import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Flow
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.Map as M

-- | Scope (of a transformer)
--   It can either be global (if we processing
--   for example global declarations and so we need
--   to change the state of the heap) or it can
--   be local to a thread and we might have to
--   change the local state and the heap
data Scope = Global | Local TId
  deriving (Show,Eq,Ord)

ppScope :: Scope -> String
ppScope Global = "@"
ppScope (Local tid) = "$"++show tid

-- | retrieves the entry node of the cfg of a function
get_entry :: String -> Graphs SymId () a -> Map SymId Symbol -> (Pos, SymId)
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
  Binary op l r     -> Binary (negOp op) l r
  Unary CNegOp e    -> e
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

instance Hashable SymId where
  hash (SymId i) = hash i
  hashWithSalt s (SymId i) = hashWithSalt s i

instance Hashable Value where
  hash v = case v of
    VInt    i -> hash i 
    VShort  i -> hash i 
    VLong   i -> hash i 
    VDouble i -> hash i 
    VFloat  i -> hash i 
    VBool   i -> hash i 
    VChar   i -> hash i 
    VString i -> hash i 
  hashWithSalt s v = case v of
    VInt    i -> hashWithSalt s i 
    VShort  i -> hashWithSalt s i 
    VLong   i -> hashWithSalt s i 
    VDouble i -> hashWithSalt s i 
    VFloat  i -> hashWithSalt s i 
    VBool   i -> hashWithSalt s i 
    VChar   i -> hashWithSalt s i 
    VString i -> hashWithSalt s i 

