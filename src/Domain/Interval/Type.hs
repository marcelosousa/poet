{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Type
-- Copyright :  (c) 2015 Marcelo Sousa
-- Implementation of the Interval Domain based on
-- Cousot & Cousot ISOP'76
-- http://www.di.ens.fr/~cousot/COUSOTpapers/publications.www/CousotCousot-ISOP-76-Dunod-p106--130-1976.pdf
-- This module defines the interval bound and the API
-------------------------------------------------------------------------------
module Domain.Interval.Type where

import Data.Hashable
import Data.List
import Util.Generic hiding (safeLookup)

data InterVal = I Int | PlusInf | MinusInf
  deriving (Show,Eq)

toInt :: InterVal -> Int
toInt i = case i of
  I n -> n
  _ -> error "toInt: can't convert PlusInf or MinusInf to int"

-- a <= b for intervals
--instance Ord (InterVal, InterVal) where
--  (<=) (a,b) (c,d) = a >= c && b <= d

instance Ord InterVal where
  (<=) (I i) PlusInf = True
  (<=) (I i) MinusInf = False
  (<=) (I i) (I j) = i <= j
  (<=) PlusInf PlusInf = True
  (<=) PlusInf _ = False
  (<=) MinusInf _ = True

instance Num InterVal where
  (+) = plusIVal
  (*) = mulIVal
  (-) = subIVal
  negate = negateIVal
  abs = absIVal
  signum = signumIVal
  fromInteger i = I $ fromInteger i

plusIVal :: InterVal -> InterVal -> InterVal
plusIVal PlusInf MinusInf = error "PlusInf  + MinusInf"
plusIVal MinusInf PlusInf = error "MinusInf + PlusInf "
plusIVal PlusInf _ = PlusInf
plusIVal MinusInf _ = MinusInf
plusIVal (I a) (I b) = I $ a + b
plusIVal a b = plusIVal b a

mulIVal :: InterVal -> InterVal -> InterVal
mulIVal PlusInf a =
  case signum a of
    I 0 -> error "PlusInf * 0"
    I (-1) -> MinusInf
    I 1 -> PlusInf
mulIVal MinusInf a =
  case signum a of
    I 0 -> error "MinusInf * 0"
    I (-1) -> PlusInf
    I 1 -> MinusInf
mulIVal (I a) (I b) = I $ a * b
mulIVal a b = mulIVal b a

subIVal :: InterVal -> InterVal -> InterVal
subIVal PlusInf MinusInf = error "PlusInf  - MinusInf"
subIVal MinusInf PlusInf = error "MinusInf - PlusInf "
subIVal PlusInf _ = PlusInf
subIVal MinusInf _ = MinusInf
subIVal _ PlusInf = MinusInf
subIVal _ MinusInf = PlusInf
subIVal (I a) (I b) = I $ a - b

absIVal :: InterVal -> InterVal
absIVal PlusInf = PlusInf
absIVal MinusInf = PlusInf
absIVal (I i) = I $ abs i

signumIVal :: InterVal -> InterVal
signumIVal PlusInf = I 1
signumIVal MinusInf = I (-1)
signumIVal (I i) = I $ signum i
    
negateIVal :: InterVal -> InterVal
negateIVal i = case i of
  I i -> I $ negate i
  PlusInf -> MinusInf
  MinusInf -> PlusInf

divide :: InterVal -> InterVal -> InterVal
divide (I 0) _ = I 0 -- error "0 / _"
divide _ PlusInf = I 0
divide _ MinusInf = I 0
divide PlusInf i = PlusInf * i
divide MinusInf i = MinusInf * i
divide (I a) (I b) = I (a `div` b)

-- 
instance Hashable InterVal where
  hash v = case v of
    I i -> hash i
    PlusInf -> hash (1::Int)
    MinusInf -> hash (0::Int)
  hashWithSalt s v = case v of 
    I i -> hashWithSalt s i
    PlusInf -> hashWithSalt s (1::Int)
    MinusInf -> hashWithSalt s (0::Int)
