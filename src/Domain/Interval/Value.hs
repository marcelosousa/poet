{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Value
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- The value for the interval semantics.
-------------------------------------------------------------------------------
module Domain.Interval.Value where

import Data.Hashable
import Data.List
import Util.Generic hiding (safeLookup)

import Domain.Util
import Domain.Interval.Type

-- | Value for the interval semantics
data IntValue
  = IntTop              -- Top value 
  | IntBot              -- Bot value
  | IntVal [Value]      -- Intcrete list of values
  | InterVal (InterVal,InterVal)  
  -- Memory address value: set of addresses
  | IntMemAddr MemAddrs 
  -- Array value
  -- Memory address for the positions and the size
  | IntArr [IntValue] Int Bool -- IsTop 
  deriving (Show,Eq)

instance Ord IntValue where
  (<=) (IntVal i) (IntVal j) = (S.fromList i) `S.isSubsetOf` (S.fromList j)
  (<=) (InterVal i) (InterVal j) = i <= j
  (<=) (Array i n _) (Array j m) = n <= m && i <= j  
  (<=) IntBot (InterVal _) = True
  (<=) IntTop IntTop = True
  (<=) IntTop (InterVal i) = i == top_interval
  (<=) (InterVal i) IntTop = i == top_interval
  (<=) _ _ = False

top_interval :: Value
top_interval = Interval (MinusInf, PlusInf)

i :: InterVal -> InterVal -> Value
i a b = InterVal (a, b)

{-
data Value = 
      IntVal Int 
    | Array [Value]
    | Interval (InterVal, InterVal)
    | Bot
  deriving (Show)
 


-- [a,b] `join` [c,d] = [min(a,c), max(b,d)]
iJoin :: Value -> Value -> Value
iJoin Bot a = a
iJoin a Bot = a
iJoin (Interval (a,b)) (Interval (c,d)) =
  Interval (min a c, max b d)
iJoin a b = 
  error $ "iJoin unsupported: " ++ show (a,b)
  
-- [a,b] `meet` [c,d] 
iMeet :: Value -> Value -> Value
iMeet Bot a = Bot
iMeet a Bot = Bot
iMeet (Interval (a,b)) (Interval (c,d)) =
  let l = max a c
      u = min b d
  in if l > u
     then Bot
     else Interval (l,u)
iMeet a b =
  error $ "iMeet unsupported: " ++ show (a,b)

instance Num Value where
  (+) = plusValue
  (*) = multValue
  (-) = subValue
  negate = negateValue
  abs = undefined
  signum = undefined
  fromInteger i = Interval (I (fromInteger i), I (fromInteger i))

plusValue :: Value -> Value -> Value
plusValue Bot Bot = Bot
plusValue Bot (Interval i) = Bot
plusValue (Interval i) Bot = Bot
plusValue (Interval (a,b)) (Interval (c,d)) = Interval (a+c, b+d)
plusValue _ _ = error "plusValue: non interval"

-- [a,b] * [c,d] = [minimum(ac,ad,bc,bd), maximum(ac,ad,bc,bd)]
multValue :: Value -> Value -> Value
multValue Bot Bot = Bot
multValue Bot (Interval i) = Bot
multValue (Interval i) Bot = Bot
multValue (Interval (a,b)) (Interval (c,d)) =
  let ac = a*c
      ad = a*d
      bc = b*c
      bd = b*d
      list = [ac,ad,bc,bd]
  in Interval (minimum list, maximum list) -- need to instantiate Eq, Ord
multValue _ _ = error "multValue: non interval"

subValue :: Value -> Value -> Value
subValue Bot Bot = Bot
subValue Bot (Interval i) = Bot
subValue (Interval i) Bot = Bot
subValue (Interval (a,b)) (Interval (c,d)) = Interval (a-d, b-c)
subValue _ _ = error "plusValue: non interval"

negateValue :: Value -> Value
negateValue v = case v of
  Interval (a,b) -> Interval (negate a, negate b)
  Bot -> Bot
  _ -> error "negateValue: non interval"

iDivide :: Value -> Value -> Value
iDivide Bot _ = Bot
iDivide _ Bot = Bot
iDivide ab cd = 
          (ab `interval_div` (cd `iMeet` (i (I 1) PlusInf))) 
  `iJoin` 
          (ab `interval_div` (cd `iMeet` (i MinusInf (I (-1)))))

-- [a,b] / [c,d] = [a,b] * 1/[c,d]
interval_div :: Value -> Value -> Value
interval_div Bot _ = Bot
interval_div _  Bot = Bot
interval_div (Interval (a,b)) (Interval (c,d)) =
  let ac = divide a c
      ad = divide a d
      bc = divide b c
      bd = divide b d
  in if c >= 1
     then i (min ac ad) (max bc bd)
  else if d <= (-1)
       then i (min bc bd) (max ac ad)
       else error "interval_div: ?"

-- [a,b] `difference` [c,d] assuming that [c,d] `join` [a,b] = [a,b]
interval_diff :: Value -> Value -> Value
interval_diff a b = interval_diff' 1 a b

interval_diff_eq :: Value -> Value -> Value
interval_diff_eq a b = interval_diff' 0 a b

interval_diff' :: Int -> Value -> Value -> Value
interval_diff' i (Interval (MinusInf, PlusInf)) (Interval (MinusInf,I b)) = Interval (I (b+i), PlusInf)
interval_diff' i (Interval (MinusInf, PlusInf)) (Interval (I a, PlusInf)) = Interval (MinusInf,I (a-i))
interval_diff' i Bot _ = Bot
interval_diff' i (Interval (a,b)) Bot = Interval (a,b)
interval_diff' i (Interval (I a,I b)) (Interval (I a', I b'))
  | a == a' && b == b' = Bot
  | a == a' = Interval (I (b'+i), I b)
  | b == b' = Interval (I a, I (a'-i))
  | a < a' && b > b' = Interval (I a,I b)
  | otherwise = error "interval_diff: fatal"
interval_diff' i a b = error $ "interval_diff: unsupported " ++ show (i,a,b)

upperBound :: Value -> InterVal
upperBound Bot = error "upperBound"
upperBound (Interval (a,b)) = b
upperBound _ = error "upperBound: unsupported"

lowerBound :: Value -> InterVal
lowerBound Bot = error "lowerBound"
lowerBound (Interval (a,b)) = a
lowerBound _ = error "lowerBound: unsupported"

-- widening
widening :: (InterVal, InterVal) -> (InterVal, InterVal) -> (InterVal, InterVal)
widening (a,b) (c,d) =
  let e = if c < a then MinusInf else a
      f = if d > b then PlusInf else b
  in (e,f)

instance Hashable Value where
  hash v = case v of
    IntVal i -> hash i
    Array vals -> hash vals
    Interval a -> hash a
    Bot -> hash (0::Int)
  hashWithSalt s v = case v of
    IntVal i -> hashWithSalt s i
    Array vals -> hashWithSalt s vals
    Interval a -> hashWithSalt s a
    Bot -> hashWithSalt s (0::Int)

-}
