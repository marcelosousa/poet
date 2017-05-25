-------------------------------------------------------------------------------
-- Module    :  Domain.Lattice
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- This module defines the class for operating with lattices
-- We usually use bottom-bounded lattices.
-------------------------------------------------------------------------------
module Domain.Lattice where
   
-- Lattice 
class Eq a => Lattice a where
   bot   :: a
   top   :: a
   join  :: a -> a -> a
   meet  :: a -> a -> a
   (<=.) :: a -> a -> Bool
   --  Is bottom?
   (?.)  :: a -> Bool
   (?.) a = a == bot