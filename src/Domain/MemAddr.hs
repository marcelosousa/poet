{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.MemAddr
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- Generic Memory Address Domain 
--  The offset of the address is another domain.
-------------------------------------------------------------------------------
module Domain.MemAddr where

import Data.Hashable
import Data.List
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Util

-- | Memory Address
data MemAddr v
  = MemAddr 
  { base   :: SymId
  , offset :: v 
  , level  :: Scope 
  }
  deriving (Show,Eq,Ord)

data MemAddrBase
  = MemAddrBase
  { _base :: SymId
  , _level :: Scope
  }
  deriving (Show,Eq,Ord)

decompose_addr :: MemAddr v -> (MemAddrBase, v)
decompose_addr a@MemAddr{..} = (MemAddrBase base level, offset)

set_offset :: MemAddr v -> v -> MemAddr v
set_offset m offset_ = m { offset = offset_ }

-- | Powerset domain of Memory Addresses
data MemAddrs v
  = MemAddrTop
  | MemAddrs [MemAddr v]
  deriving (Eq)

instance Show v => Show (MemAddrs v) where
  show a = case a of
    MemAddrTop -> "MemAddrTop"
    MemAddrs l -> show l

-- Ordering operation
instance Ord v => Ord (MemAddrs v) where
  m1 <= m2 = case (m1,m2) of 
    (_,MemAddrTop) -> True
    (MemAddrTop,MemAddrs l) -> False 
    (MemAddrs l1,MemAddrs l2) ->
      all (\a -> a `elem` l2) l1 

-- | Bottom element
bot_maddrs :: MemAddrs v
bot_maddrs = MemAddrs []

-- | Check for bottom
is_maddrs_bot :: MemAddrs v -> Bool
is_maddrs_bot maddr =
  case maddr of
    MemAddrTop -> False
    MemAddrs l -> null l

-- | Meet operation
meet_maddrs :: Eq v => MemAddrs v -> MemAddrs v -> MemAddrs v
meet_maddrs a1 a2 =
  case (a1,a2) of
    (MemAddrTop,_) -> a2
    (_,MemAddrTop) -> a1
    (MemAddrs l1, MemAddrs l2) -> 
      MemAddrs (l1 `intersect` l2)

-- | Join operation
join_maddrs :: Eq v => MemAddrs v -> MemAddrs v -> MemAddrs v
join_maddrs a1 a2 =
  case (a1,a2) of
    (MemAddrTop,_) -> a1
    (_,MemAddrTop) -> a2
    (MemAddrs l1, MemAddrs l2) ->
      MemAddrs (nub $ l1 ++ l2)

-- | Check if memory addresses are at the global scope
--   Useful for? 
is_global :: MemAddrs v -> Bool
is_global maddr = case maddr of
  MemAddrTop -> True
  MemAddrs l -> any (\a@MemAddr{..} -> level == Global) l

-- | Hashable instances
instance Hashable v => Hashable (MemAddrs v) where
  hash m = case m of
    MemAddrTop -> hash (0::Int)
    MemAddrs l -> hash l
  hashWithSalt s m = case m of
    MemAddrTop -> hashWithSalt s (0::Int)
    MemAddrs l -> hashWithSalt s l

-- @TODO Probably has to change
instance Hashable v => Hashable (MemAddr v) where
  hash m@MemAddr{..} = hash base
  hashWithSalt s m@MemAddr{..} = hashWithSalt s base

