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
import Domain.Lattice
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Util

-- | Memory Address
data MemAddr d
  = MemAddr 
  { base   :: SymId
  , offset :: d 
  , level  :: Scope 
  }
  deriving (Eq,Ord)

instance Show d => Show (MemAddr d) where
  show (MemAddr base offset level) = show base
  
data MemAddrBase
  = MemAddrBase
  { _base  :: SymId
  , _level :: Scope
  }
  deriving (Show,Eq,Ord)

from_addr :: MemAddr v -> (MemAddrBase, v)
from_addr a@MemAddr{..} = (MemAddrBase base level, offset)

to_addr :: MemAddrBase -> v -> MemAddr v
to_addr addr v = MemAddr (_base addr) v (_level addr)
 
set_level :: MemAddrBase -> Scope -> MemAddrBase
set_level addr scope = MemAddrBase (_base addr) scope

set_offset :: MemAddr v -> v -> MemAddr v
set_offset m offset_ = m { offset = offset_ }

-- | Powerset domain of Memory Addresses
data MemAddrs v
  = MemAddrTop
  | MemAddrs [MemAddr v]
  deriving (Eq)

-- | Check if memory addresses are at the global scope
--   Useful for? 
is_global :: MemAddrs v -> Bool
is_global maddr = case maddr of
  MemAddrTop -> True
  MemAddrs l -> any (\a@MemAddr{..} -> level == Global) l
  
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

-- Lattice definition
instance (Eq v, Ord v) => Lattice (MemAddrs v) where
   bot    = MemAddrs []
   top    = MemAddrTop
   meet a b = case (a,b) of
    (MemAddrTop,_) -> b
    (_,MemAddrTop) -> a
    (MemAddrs l1, MemAddrs l2) -> 
      MemAddrs (l1 `intersect` l2)
   join a b = case (a,b) of
    (MemAddrTop,_) -> a
    (_,MemAddrTop) -> b
    (MemAddrs l1, MemAddrs l2) ->
      MemAddrs (nub $ l1 ++ l2)

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

instance Hashable MemAddrBase where
  hash m@MemAddrBase{..} = hash _base
  hashWithSalt s m@MemAddrBase{..} = hashWithSalt s _base