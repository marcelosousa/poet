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
import Model.GCS
import Language.SimpleC.AST
import Language.SimpleC.Util
 
-- | Scope (of a transformer)
--   It can either be global (if we processing
--   for example global declarations and so we need
--   to change the state of the heap) or it can
--   be local to a thread and we might have to
--   change the local state and the heap
data Scope = Global | Local TId
  deriving (Show,Eq,Ord)

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

