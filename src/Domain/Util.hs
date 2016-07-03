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
import Data.List
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

-- | Concrete Memory address contains of a base + offset
-- data MemAddr
--   = MemAddr 
--   { base :: ConValue
--   , offset :: ConValue
--   }
--   deriving (Show,Eq,Ord)
-- Simplification
-- @Add Scope to MemAddr!
data MemAddr
  = MemAddr 
  { base :: SymId
  , level :: Scope }
  deriving (Show,Eq,Ord)

data MemAddrs
  = MemAddrTop
  | MemAddrs [MemAddr]
  deriving (Show,Eq)

instance Ord MemAddrs where
  m1 <= m2 = case (m1,m2) of 
    (_,MemAddrTop) -> True
    (MemAddrTop,MemAddrs l) -> False 
    (MemAddrs l1,MemAddrs l2) ->
      all (\a -> a `elem` l2) l1 

bot_maddrs :: MemAddrs
bot_maddrs = MemAddrs []

is_maddrs_bot :: MemAddrs -> Bool
is_maddrs_bot maddr =
  case maddr of
    MemAddrTop -> False
    MemAddrs l -> null l
  
meet_maddrs :: MemAddrs -> MemAddrs -> MemAddrs
meet_maddrs a1 a2 =
  case (a1,a2) of
    (MemAddrTop,_) -> a2
    (_,MemAddrTop) -> a1
    (MemAddrs l1, MemAddrs l2) -> 
      MemAddrs (l1 `intersect` l2)

join_maddrs :: MemAddrs -> MemAddrs -> MemAddrs
join_maddrs a1 a2 =
  case (a1,a2) of
    (MemAddrTop,_) -> a1
    (_,MemAddrTop) -> a2
    (MemAddrs l1, MemAddrs l2) ->
      MemAddrs (nub $ l1 ++ l2)
 
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

instance Hashable MemAddr where
  hash m@MemAddr{..} = hash base
  hashWithSalt s m@MemAddr{..} = hashWithSalt s base

{- 
-- State equality: because I'm using a hashtable I need to stay within the ST monad
isEqual :: Sigma s -> Sigma s -> ST s Bool
isEqual s1 s2 = do
  l1 <- H.toList s1
  l2 <- H.toList s2
  return $ isEqual' l1 l2  
-}
