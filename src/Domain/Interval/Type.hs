{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Type
-- Copyright :  (c) 2015 Marcelo Sousa
-- Implementation of the Interval Domain based on
-- Cousot & Cousot ISOP'76
-- http://www.di.ens.fr/~cousot/COUSOTpapers/publications.www/CousotCousot-ISOP-76-Dunod-p106--130-1976.pdf
-------------------------------------------------------------------------------
module Domain.Interval.Type where

import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Util.Generic hiding (safeLookup)
import Debug.Trace
type ISigma = Sigma
type Sigma = Map Var Value

-- Local State
type LSigma = [(Var,Value)]

data Value = 
      IntVal Int 
    | Array [Value]
    | Interval (InterVal, InterVal)
    | Top | Bot
  deriving (Show,Ord)

equals :: Value -> Value -> Bool
equals Top Top = True
equals Top (Interval (MinusInf, PlusInf)) = True
equals Top _ = False
equals Bot Bot = True
equals Bot _ = False
equals (IntVal i) (IntVal j) = i == j
equals (IntVal i) _ = False
equals (Array vis) (Array vjs) = vis == vjs
equals (Array vis) _ = False
equals (Interval (a,b)) (Interval (c,d)) = (a,b) == (c,d)
equals a b = equals b a

instance Eq Value where
  (==) (IntVal i) (IntVal j) = i == j
  (==) (IntVal i) _ = False
  (==) (Array vis) (Array vjs) = vis == vjs
  (==) (Array vis) _ = False
  (==) _ (IntVal j) = False
  (==) _ (Array vjs) = False
  (==) a b = a `subsumes` b

data InterVal = I Int | PlusInf | MinusInf
  deriving (Show,Eq,Ord)

-- TODO:
-- missing deriving Eq and Ord
-- instance Num InterVal where
-- instance Domain InterVal 

-- [a,b] `join` [c,d] = [min(a,c), max(b,d)]
interval_join :: Value -> Value -> Value
interval_join Top a = Top
interval_join a Top = Top
interval_join Bot a = a
interval_join a Bot = a
interval_join (Interval (a,b)) (Interval (c,d)) =
  Interval $ interval_join' (a,b) (c,d)
interval_join a b = 
  error $ "interval_join unsupported: " ++ show (a,b)
  
interval_join' :: (InterVal, InterVal) -> (InterVal, InterVal) -> (InterVal, InterVal)
interval_join' (a,b) (c,d) = (interVal_min a c, interVal_max b d)

-- [a,b] `meet` [c,d] 
interval_meet :: Value -> Value -> Value
interval_meet Top a = a
interval_meet a Top = a
interval_meet Bot a = Bot
interval_meet a Bot = Bot
interval_meet (Interval (a,b)) (Interval (c,d)) = trace ("Meet of " ++ show ((Interval (a,b),Interval (c,d)))) $
  let l = interVal_max a c
      u = interVal_min b d
  in if l `gt` u
     then trace ("Returning Bot") $ Bot
     else Interval (l,u)
interval_meet a b = error $ "interval_meet unsupported: " ++ show (a,b)

-- [a,b] `difference` [c,d] assuming that [c,d] `join` [a,b] = [a,b]
interval_diff :: Value -> Value -> Value
interval_diff a b = interval_diff' 1 a b

interval_diff_eq :: Value -> Value -> Value
interval_diff_eq a b = interval_diff' 0 a b

interval_diff' :: Int -> Value -> Value -> Value
interval_diff' i Top Top = Bot
interval_diff' i Top Bot = Top
interval_diff' i Top (Interval (MinusInf,I b)) = Interval (I (b+i), PlusInf)
interval_diff' i Top (Interval (I a, PlusInf)) = Interval (MinusInf, I (a-i))
interval_diff' i Top a = interval_diff' i (Interval (MinusInf, PlusInf)) a
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
interval_diff' i a b = Top --error $ "interval_diff: unsupported " ++ show (i,a,b)

upperBound :: Value -> InterVal
upperBound Top = PlusInf
upperBound Bot = error "upperBound"
upperBound (Interval (a,b)) = b
upperBound _ = error "upperBound: unsupported"

lowerBound :: Value -> InterVal
lowerBound Top = PlusInf
lowerBound Bot = error "lowerBound"
lowerBound (Interval (a,b)) = a
lowerBound _ = error "lowerBound: unsupported"

interVal_min :: InterVal -> InterVal -> InterVal
interVal_min PlusInf a = a
interVal_min MinusInf _ = MinusInf
interVal_min (I a) (I b) = I $ min a b
interVal_min a b = trace "bla" $ interVal_min b a

interVal_max :: InterVal -> InterVal -> InterVal
interVal_max PlusInf a = PlusInf
interVal_max MinusInf a = a
interVal_max (I a) (I b) = I $ max a b
interVal_max a b = interVal_max b a

-- a <= b <=> a `join` b = b
subsumes :: Value -> Value -> Bool
subsumes a b = (interval_join a b) `equals` b

-- a < b <=> a <= b && a != b
strictly_subsumes :: Value -> Value -> Bool
strictly_subsumes a b = subsumes a b && (not $ equals a b)

-- widening
widening :: (InterVal, InterVal) -> (InterVal, InterVal) -> (InterVal, InterVal)
widening (a,b) (c,d) =
  let e = if c `lt` a then MinusInf else a
      f = if d `gt` b then PlusInf else b
  in (e,f)

-- a `less than` b
lt :: InterVal -> InterVal -> Bool
lt MinusInf _ = True
lt _ PlusInf = True
lt (I a) MinusInf = False
lt PlusInf (I a) = False
lt (I a) (I b) = a < b

-- a `greater than` b
gt :: InterVal -> InterVal -> Bool
gt _ MinusInf = True
gt PlusInf _ = True
gt (I a) PlusInf = False
gt MinusInf (I a) = False
gt (I a) (I b) = a > b


-- Interval Arithmetic
-- needs to be fixed so that PlusInf + MinusInf = Top
interVal_op :: (Int -> Int -> Int) -> InterVal -> InterVal -> InterVal
interVal_op op MinusInf PlusInf = error "MinusInf + PlusInf is undefined"
interVal_op op MinusInf a = MinusInf
interVal_op op PlusInf a = PlusInf
interVal_op op (I a) (I b) = I $ op a b
interVal_op op a b = interVal_op op b a

-- clean this up to use the signum
interVal_mult :: InterVal -> InterVal -> InterVal
interVal_mult MinusInf PlusInf = MinusInf
interVal_mult MinusInf MinusInf = PlusInf
interVal_mult PlusInf PlusInf = PlusInf
interVal_mult (I a) (I b) = I $ a * b
interVal_mult (I a) inf | a > 0 = inf
                        | a < 0 = reverseInf inf
                        | otherwise = error "0 * Inf"
interVal_mult a b = interVal_mult b a

reverseInf :: InterVal -> InterVal
reverseInf MinusInf = PlusInf
reverseInf PlusInf = MinusInf
reverseInf _ = error "reverseInf"

-- [a,b] + [c,d] = [a+c, b+d]
interval_add :: (InterVal, InterVal) -> (InterVal, InterVal) -> (InterVal, InterVal)
interval_add (a,b) (c,d) = (interVal_op (+) a c, interVal_op (+) b d)

-- [a,b] - [c,d] = [a-d, b-c]
interval_sub :: (InterVal, InterVal) -> (InterVal, InterVal) -> (InterVal, InterVal)
interval_sub (a,b) (c,d) = (interVal_op (-) a c, interVal_op (-) b d)

-- [a,b] * [c,d] = [minimum(ac,ad,bc,bd), maximum(ac,ad,bc,bd)]
interval_mult :: (InterVal, InterVal) -> (InterVal, InterVal) -> (InterVal, InterVal)
interval_mult (a,b) (c,d) =
  let ac = interVal_mult a c
      ad = interVal_mult a d
      bc = interVal_mult b c
      bd = interVal_mult b d
      list = [ac,ad,bc,bd]
  in (minimum list, maximum list) -- need to instantiate Eq, Ord

-- [a,b] / [c,d] = [a,b] * 1/[c,d]
interval_div :: (InterVal, InterVal) -> (InterVal, InterVal) -> (InterVal, InterVal)
interval_div (a,b) (c,d) =
  let (c',d') = int_div 1 (c,d)
  in interval_mult (a,b) (c',d')

-- this can be made more precise by providing two intervals
int_div :: Int -> (InterVal, InterVal) -> (InterVal, InterVal)
int_div i (I a,I b) =
  if a <= 0 && b >= 0
  then (MinusInf, PlusInf)
  else (I $ i `div` a,I $ i `div` b)

instance Hashable Sigma where
  hash = hash . M.toList
  hashWithSalt s st = hashWithSalt s $ M.toList st
  
instance Hashable Value where
  hash v = case v of
    IntVal i -> hash i
    Array vals -> hash vals
    Interval a -> hash a
    Top -> hash (1::Int)
    Bot -> hash (0::Int)
  hashWithSalt s v = case v of
    IntVal i -> hashWithSalt s i
    Array vals -> hashWithSalt s vals
    Interval a -> hashWithSalt s a
    Top -> hashWithSalt s (1::Int)
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
