{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Type
-- Copyright :  (c) 2015 Marcelo Sousa
-- Implementation of the Interval Domain based on
-- Cousot & Cousot ISOP'76
-- http://www.di.ens.fr/~cousot/COUSOTpapers/publications.www/CousotCousot-ISOP-76-Dunod-p106--130-1976.pdf
-------------------------------------------------------------------------------
module Domain.Interval.Type where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Model.GCS
import Util.Generic hiding (safeLookup)
import Debug.Trace

type ISigma = Sigma
type Sigma = Map Var Value

instance Projection Sigma where
  controlPart =
    M.filterWithKey (\k _ -> isPC k)
  dataPart = 
    M.filterWithKey (\k _ -> not $ isPC k)
  subsumes g l = 
    M.isSubmapOfBy (<=) l g

isPC :: Var -> Bool
isPC v = 
  case BSC.split '.' v of
    [_,x] -> let x' = BSC.unpack x
             in x' == "pc"
    _ -> False

-- Local State
type LSigma = [(Var,Value)]

data Value = 
      IntVal Int 
    | Array [Value]
    | Interval (InterVal, InterVal)
    | Bot
  deriving (Show)
 
instance Eq Value where
  (==) (IntVal i) (IntVal j) = i == j
  (==) (Array i) (Array j) = i == j  
  (==) (Interval a) (Interval b) = a == b
  (==) Bot Bot = True
  (==) _ _ = False

instance Ord Value where
  (<=) (IntVal i) (IntVal j) = i <= j
  (<=) (Array i) (Array j) = i <= j  
  (<=) (Interval a) (Interval b) = a <= b
  (<=) Bot (Interval _) = True
  (<=) _ _ = False

data InterVal = I Int | PlusInf | MinusInf
  deriving (Show,Eq)

-- a <= b for intervals
instance Ord (InterVal, InterVal) where
  (<=) (a,b) (c,d) = a >= c && b <= d

instance Ord InterVal where
  (<=) (I i) PlusInf = True
  (<=) (I i) MinusInf = False
  (<=) (I i) (I j) = i <= j
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
divide (I 0) _ = error "0 / _"
divide _ PlusInf = I 0
divide _ MinusInf = I 0
divide PlusInf i = PlusInf * i
divide MinusInf i = MinusInf * i
divide (I a) (I b) = I (a `div` b)

top :: Value
top = Interval (MinusInf, PlusInf)

i :: InterVal -> InterVal -> Value
i a b = Interval (a, b)

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

-----

instance Hashable Sigma where
  hash = hash . M.toList
  hashWithSalt s st = hashWithSalt s $ M.toList st
  
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

instance Hashable InterVal where
  hash v = case v of
    I i -> hash i
    PlusInf -> hash (1::Int)
    MinusInf -> hash (0::Int)
  hashWithSalt s v = case v of 
    I i -> hashWithSalt s i
    PlusInf -> hashWithSalt s (1::Int)
    MinusInf -> hashWithSalt s (0::Int)

toSigma :: LSigma -> Sigma
toSigma = M.fromList

insert :: Var -> Value -> Sigma -> Sigma
insert = M.insert

join :: Sigma -> Sigma -> Sigma
join = M.union

safeLookup :: String -> Sigma -> Var -> Value
safeLookup err st k =
  case M.lookup k st of 
    Nothing -> error $ "safeLookup: " ++ err
    Just v  -> v

showSigma :: Sigma -> String
showSigma s =
  let kv = M.toList s
  in showSigma' kv 

showSigma' :: [(Var, Value)] -> String
showSigma' [] = ""
showSigma' ((v,vs):rest) = show v ++ "=" ++ show vs ++ "\n" ++ showSigma' rest