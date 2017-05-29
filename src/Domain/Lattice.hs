-------------------------------------------------------------------------------
-- Module    :  Domain.Lattice
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- This module defines the class for operating with lattices
-- We usually use bottom-bounded lattices.
-------------------------------------------------------------------------------
module Domain.Lattice where
   
import Data.List

-- Lattice 
class (Eq a, Ord a) => Lattice a where
   bot   :: a
   top   :: a
   join  :: a -> a -> a
   meet  :: a -> a -> a
   (<=.) :: a -> a -> Bool
   (<=.) = (<=)
   --  Is bottom?
   (?.)  :: a -> Bool
   (?.) a = a == bot
   -- Join List
   joinL :: [a] -> a
   joinL [] = error "joinL: empty list"
   joinL [x] = x
   joinL (x:xs) = x `join` (joinL xs)
   -- Widening
   widen :: a -> a -> a
   widen = error "widening for this domain not implemented"

{-
instance Ord a => Lattice [a] where
   bot  = []
   top  = error "No top element for a list"
   join = (++) 
   meet = intersect
-}