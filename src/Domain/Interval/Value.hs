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
import Domain.Action
import Domain.Interval.Type
import Domain.MemAddr
import Domain.Lattice
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.Set as S

-- | Interval Action
type IntAct      = Act IntValue

-- | Interval Memory Addr
type IntMAddr    = MemAddr IntValue
type IntMAddrs   = MemAddrs IntValue

-- | Value for the interval semantics
data IntValue
  = IntTop              -- Top value 
  | IntBot              -- Bot value
  | IntVal [Value]      -- "Intcrete" list of values
  | InterVal (InterVal, InterVal)  
  -- Memory address value: set of addresses
  | IntMemAddr IntMAddrs 
  deriving (Show,Eq)

-- Quick builders
i :: InterVal -> InterVal -> IntValue
i a b = InterVal (a, b)

instance ToValue IntValue where
   kVal n = i (I n) (I n)

instance Ord IntValue where
  (<=) (IntVal i) (IntVal j) = (S.fromList i) `S.isSubsetOf` (S.fromList j)
  (<=) (InterVal (a,b)) (InterVal (c,d)) = a >= c && b <= d
  (<=) IntBot _ = True
  (<=) IntTop (InterVal i) = i == (MinusInf, PlusInf)
  (<=) a IntTop = True
  (<=) _ _ = False

-- Lattice definition
instance Lattice IntValue where
   bot   = IntBot
   top   = InterVal (MinusInf, PlusInf)
   join v1 v2 = case (v1,v2) of
     (IntBot,a)                      -> a
     (a,IntBot)                      -> a
     (IntTop,a)                      -> IntTop
     (a,IntTop)                      -> IntTop
     (IntVal i,IntVal j)             -> IntVal $ nub $ i ++ j 
     (IntMemAddr m1,IntMemAddr m2)   -> IntMemAddr (m1 `join` m2 )
     (InterVal (a,b),InterVal (c,d)) -> InterVal (min a c, max b d)
     _ -> error $ "join unsupported: " ++ show (v1,v2)
   meet  v1 v2 = case (v1,v2) of
     (IntBot,a)                      -> IntBot
     (a,IntBot)                      -> IntBot
     (IntTop,a)                      -> a 
     (a,IntTop)                      -> a 
     (IntVal i,IntVal j)             -> IntVal $ i `intersect` j 
     (IntMemAddr m1,IntMemAddr m2)   -> IntMemAddr (m1 `meet` m2)
     (InterVal (a,b),InterVal (c,d)) -> 
       let l = max a c
           u = min b d
       in if l > u
          then IntBot
          else InterVal (l,u)
     _ -> error $ "meet unsupported: " ++ show (v1,v2)
   widen v1 v2 = 
     if v1 == v2
     then v1
     else case (v1, v2) of
       (InterVal (a,b), InterVal (c,d)) ->
         let e = if c < a then MinusInf else a
             f = if d > b then PlusInf else b
         in InterVal (e,f)
       (_,_) -> error $ "widen: not supported IntVals = " ++ show (v1, v2)

instance Num IntValue where
  (+) v1 v2 = 
    case (v1,v2) of
      (IntBot,_) -> IntBot
      (_,IntBot) -> IntBot
      (IntTop,_) -> IntTop
      (_,IntTop) -> IntTop
      (InterVal (a,b), InterVal (c,d)) -> InterVal (a+c, b+d)
      (IntVal i,IntVal j) -> IntVal [add_value a b | a <- i, b <- j]
      _ -> error "plusValue: non interval"
  -- [a,b] * [c,d] = [minimum(ac,ad,bc,bd), maximum(ac,ad,bc,bd)]
  (*) v1 v2 = 
    case (v1,v2) of
      (IntBot,_) -> IntBot
      (_,IntBot) -> IntBot
      (IntTop,_) -> IntTop
      (_,IntTop) -> IntTop
      (InterVal (a,b), InterVal (c,d)) ->
        let ac = a*c
            ad = a*d
            bc = b*c
            bd = b*d
            list = [ac,ad,bc,bd]
        in InterVal (minimum list, maximum list) 
      (IntVal i,IntVal j) -> IntVal [mult_value a b | a <- i, b <- j]
      _ -> error "multValue: non interval"
  (-) v1 v2 = 
    case (v1,v2) of
      (IntBot,_) -> IntBot
      (_,IntBot) -> IntBot
      (IntTop,_) -> IntTop
      (_,IntTop) -> IntTop
      (InterVal (a,b), InterVal (c,d)) -> InterVal (a-d, b-c)
      (IntVal i,IntVal j) -> IntVal [sub_value a b | a <- i, b <- j]
      _ -> error "subValue: non interval"
  negate v = 
    case v of
      IntBot -> IntBot
      IntTop -> IntTop
      InterVal (a,b) -> InterVal (negate a, negate b)
      IntVal i -> IntVal [minus_value a | a <- i]
      _ -> error "negateValue: non interval"
  abs = error "abs for intervals unsupported" 
  signum = error "signum for intervals unsupported"
  fromInteger i = InterVal (I (fromInteger i), I (fromInteger i))


-- | ?
iDivide :: IntValue -> IntValue -> IntValue
iDivide v1 v2 = case (v1,v2) of
  (IntBot,_) -> IntBot
  (_,IntBot) -> IntBot
  (IntTop,_) -> IntTop
  (_,IntTop) -> IntTop
  (IntVal i,IntVal j) -> IntVal [div_value a b | a <- i, b <- j] 
  (ab,cd) ->
    let res = (v1 `interval_div` (v2 `meet` (i (I 1) PlusInf))) 
              `join` 
              (v1 `interval_div` (v2 `meet` (i MinusInf (I (-1)))))
    in res 

-- [a,b] / [c,d] = [a,b] * 1/[c,d]
interval_div :: IntValue -> IntValue -> IntValue
interval_div v1 v2 = mytrace False ("interval_div: " ++ show v1 ++ " div " ++ show v2) $ case (v1,v2) of 
  (IntBot,_) -> IntBot
  (_,IntBot) -> IntBot
  (IntTop,_) -> IntTop
  (_,IntTop) -> IntTop
  (InterVal (a,b),InterVal (c,d)) ->
    let ac = divide a c
        ad = divide a d
        bc = divide b c
        bd = divide b d
    in if c >= 1
       then i (min ac ad) (max bc bd)
    else if d <= (-1)
         then i (min bc bd) (max ac ad)
         else error "interval_div: ?"
  _ -> error "interval_div: not supported"

-- [a,b] `difference` [c,d] assuming that [c,d] `join` [a,b] = [a,b]
interval_diff :: IntValue -> IntValue -> IntValue
interval_diff a b = interval_diff' 1 a b

interval_diff_eq :: IntValue -> IntValue -> IntValue
interval_diff_eq a b = interval_diff' 0 a b

interval_diff' :: Int -> IntValue -> IntValue -> IntValue
interval_diff' i (InterVal (MinusInf, PlusInf)) (InterVal (MinusInf,I b)) = 
  InterVal (I (b+i), PlusInf)
interval_diff' i (InterVal (MinusInf, PlusInf)) (InterVal (I a, PlusInf)) =
  InterVal (MinusInf,I (a-i))
interval_diff' i IntBot _ = IntBot
interval_diff' i (InterVal (a,b)) IntBot = InterVal (a,b)
interval_diff' i (InterVal (I a,I b)) (InterVal (I a', I b'))
  | a == a' && b == b' = IntBot
  | a == a' = InterVal (I (b'+i), I b)
  | b == b' = InterVal (I a, I (a'-i))
  | a < a' && b > b' = InterVal (I a,I b)
  | otherwise = error "interval_diff: fatal"
interval_diff' i a b = error $ "interval_diff: unsupported " ++ show (i,a,b)

upperBound :: IntValue -> InterVal
upperBound IntBot = error "upperBound: IntBot"
upperBound (InterVal (a,b)) = b
upperBound IntTop = PlusInf
upperBound (IntVal vals) =
  case maximum vals of
    VInt n -> I n
    _ -> error $ "upperBound: unsupported IntVal " ++ show vals 
upperBound _ = error "upperBound: unsupported"

lowerBound :: IntValue -> InterVal
lowerBound IntBot = error "lowerBound"
lowerBound IntTop = MinusInf
lowerBound (InterVal (a,b)) = a
lowerBound (IntVal vals) =
  case minimum vals of
    VInt n -> I n
    _ -> error $ "lowerBound: unsupported IntVal " ++ show vals 
lowerBound _ = error "lowerBound: unsupported"

-- diff_intval a@[l1, u1] b@[l2, u2] 
-- pre-condition: a > b
diff_intval :: IntValue -> IntValue -> IntValue
diff_intval (InterVal (_,u)) (InterVal (_,l)) = InterVal (l+1,u)

instance Hashable IntValue where
  hash v = case v of
    IntVal i -> hash i
    InterVal a -> hash a
    IntBot -> hash (0::Int)
    IntTop -> hash (1::Int)
    IntMemAddr mem -> hash mem 
  hashWithSalt s v = case v of
    IntVal i -> hashWithSalt s i
    InterVal a -> hashWithSalt s a
    IntBot -> hashWithSalt s (0::Int)
    IntTop -> hashWithSalt s (1::Int)
    IntMemAddr mem -> hashWithSalt s mem 
