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
import Domain.Interval.Type
import Domain.Util
import Domain.Action
import Domain.MemAddr
import Language.SimpleC.Util
import Util.Generic hiding (safeLookup)
import qualified Data.Set as S

-- | Interval Action
type IntAct = Act IntValue

-- | Interval Memory Addr
type IntMAddr = MemAddr IntValue
type IntMAddrs = MemAddrs IntValue

type IntOffsList = [(IntValue, IntValue)]

-- | Value for the interval semantics
data IntValue
  = IntTop              -- Top value 
  | IntBot              -- Bot value
  | IntVal [Value]      -- Intcrete list of values
  | InterVal (InterVal, InterVal)  
  -- Memory address value: set of addresses
  | IntMemAddr IntMAddrs 
  -- Array value
  -- Memory address for the positions and the size
  | IntArr [IntValue] Int Bool -- IsTop 
  deriving (Show,Ord,Eq)

concretize_interval :: IntValue -> [IntValue]
concretize_interval i = case i of
  InterVal (l, u) -> 
    let lb = toInt l
        ub = toInt u
    in map k [lb..ub] 
  _ -> error $ "concretize_interval: not supported " ++ show i

is_interval :: IntValue -> Bool 
is_interval i = case i of
  IntBot -> True 
  IntVal l -> length l /= 1
  InterVal (l, u) ->
    case (l, u) of 
      (I lb, I ub) -> ub /= lb
      _ -> True
  _ -> error $ "range: " ++ show i ++ " not supported"

zero :: IntValue
zero = InterVal (I 0,I 0) 

k :: Int -> IntValue
k n = InterVal (I n, I n)

one :: IntValue
one = InterVal (I 1, I 1)

join_intval_list :: [IntValue] -> IntValue
join_intval_list [] = error "join_intval_list: empty list"
join_intval_list [x] = x
join_intval_list (x:xs) = x `iJoin` (join_intval_list xs)  

-- instance Ord IntValue where
--   (<=) (IntVal i) (IntVal j) = (S.fromList i) `S.isSubsetOf` (S.fromList j)
--   (<=) (InterVal (a,b)) (InterVal (c,d)) = a >= c && b <= d
--   (<=) (IntArr i n _) (IntArr j m _) = n <= m && i <= j  
--   (<=) IntBot (InterVal _) = True
--   (<=) IntTop IntTop = True
--   (<=) IntTop (InterVal i) = i == (MinusInf, PlusInf) 
--   (<=) (InterVal i) IntTop = i == (MinusInf, PlusInf) 
--   (<=) _ _ = False

-- Functions over two lists of interval offsets
--  Subsumption
--  Join
--  Widening

-- X1 >= X2
subsumes_intval_list :: IntOffsList -> IntOffsList -> Bool
subsumes_intval_list _x1 _x2 =
  case (_x1, _x2) of
    ([], []) -> True
    ([], _) -> False
    (_, []) -> True
    ((a1,v1):x1, (b1,u1):x2) ->
      if v1 < u1
      then False
      else if a1 == b1
           then subsumes_intval_list x1 x2 
           else if a1 < b1 
                then subsumes_intval_list x1 ((diff_intval b1 a1,u1):x2)
                else if a1 > b1
                     then subsumes_intval_list ((diff_intval a1 b1,v1):x1) x2
                     else False

-- diff_intval a@[l1, u1] b@[l2, u2] 
-- pre-condition: a > b
-- 
diff_intval :: IntValue -> IntValue -> IntValue
diff_intval (InterVal (_,u)) (InterVal (_,l)) = InterVal (l+1,u)

-- Generic function that matches 
-- the offset intervals and applies a function over its values

gen_intval_list :: (IntValue -> IntValue -> IntValue) -> IntOffsList -> IntOffsList -> IntOffsList
gen_intval_list fn _x1 _x2 = 
  case (_x1, _x2) of
    ([], _) -> _x2 
    (_, []) -> _x1
    ((a,v):x1, (b,u):x2) ->
      case (a, b) of
        (InterVal (a1,a2), InterVal (b1,b2)) ->
          case compare a1 b1 of
            -- 1. a1 == b1
            EQ -> case compare a2 b2 of
              -- i. a2 == b2 
              EQ -> (a,v `fn` u):(gen_intval_list fn x1 x2)
              -- ii. a2 < b2
              LT -> 
                let b' = InterVal (a2 + 1, b2)
                in (a,v `fn` u):(gen_intval_list fn x1 ((b',u):x2))
               -- iii. a2 > b2
              GT -> 
                let a' = InterVal (b2 + 1, a2)
                in (b,v `fn` u):(gen_intval_list fn ((a',v):x1) _x2)
            -- 2. a1 < b1
            LT -> case compare a2 b2 of
              -- i. a2 == b2
              EQ ->
                let a' = InterVal (a1, b1 - 1)
                in (a',v):(b, v `fn` u):(gen_intval_list fn x1 x2)
              -- ii. a2 < b2
              LT -> case compare a2 b1 of
                -- a. a2 < b1
                LT -> (a,v):(gen_intval_list fn x1 _x2)
                -- b. a2 >= b1
                _ ->
                  let a' = InterVal (a1, b1 - 1)
                      ab = InterVal (b1, a2)
                      b' = InterVal (a2 + 1, b2)
                  in (a',v):(ab, v `fn` u):(gen_intval_list fn x1 ((b',v):x2))
              -- iii. a2 > b2
              GT ->  
                let ab = InterVal (a1, b1 - 1)
                    a' = InterVal (b2 + 1, a2)
                in (ab,v):(b, v `fn` u):(gen_intval_list fn ((a',u):x1) x2)
            -- 3. b1 < a1
            GT -> case compare b2 a2 of
              -- i. b2 == a2
              EQ ->
                let b' = InterVal (b1, a1 - 1)
                in (b',u):(a, v `fn` u):(gen_intval_list fn x1 x2)
              -- ii. b2 < a2
              LT -> case compare b2 a1 of
                -- a. b2 < a1
                LT -> (b,v):(gen_intval_list fn _x1 x2)
                -- b. b2 >= a1
                _ ->
                  let b' = InterVal (b1, a1 - 1)
                      ba = InterVal (a1, b2)
                      a' = InterVal (b2 + 1, a2)
                  in (b',u):(ba, v `fn` u):(gen_intval_list fn ((a',u):x1) x2)
              -- iii. b2 > a2
              GT ->  
                let ba = InterVal (b1, a1 - 1)
                    b' = InterVal (a2 + 1, b2)
                in (ba,u):(a, v `fn` u):(gen_intval_list fn x1 ((b',v):x2))

-- X1 join X2
join_intval_lists :: IntOffsList -> IntOffsList -> IntOffsList 
join_intval_lists = gen_intval_list iJoin 

-- X1 widen X2
widen_intval_lists :: IntOffsList -> IntOffsList -> IntOffsList 
widen_intval_lists = gen_intval_list iWiden 

top_interval :: IntValue
top_interval = InterVal (MinusInf, PlusInf)

i :: InterVal -> InterVal -> IntValue
i a b = InterVal (a, b)

-- widening
iWiden :: IntValue -> IntValue -> IntValue
iWiden v1 v2 = 
  if v1 == v2
  then v1
  else case (v1, v2) of
    (InterVal (a,b), InterVal (c,d)) ->
      let e = if c < a then MinusInf else a
          f = if d > b then PlusInf else b
      in InterVal (e,f)
    (_,_) -> error $ "iWiden: not supported IntVals = " ++ show (v1, v2)

-- [a,b] `join` [c,d] = [min(a,c), max(b,d)]
iJoin :: IntValue -> IntValue -> IntValue
iJoin v1 v2 = case (v1,v2) of
  (IntBot,a) -> a
  (a,IntBot) -> a
  (IntTop,a) -> IntTop
  (a,IntTop) -> IntTop
  (IntVal i,IntVal j) -> IntVal $ nub $ i ++ j 
  (IntMemAddr m1,IntMemAddr m2) -> IntMemAddr $ m1 `join_maddrs` m2 
  (InterVal (a,b),InterVal (c,d)) -> InterVal (min a c, max b d)
  _ -> error $ "iJoin unsupported: " ++ show (v1,v2)
 
iMeet :: IntValue -> IntValue -> IntValue
iMeet v1 v2 = case (v1,v2) of
  (IntBot,a) -> IntBot
  (a,IntBot) -> IntBot
  (IntTop,a) -> a 
  (a,IntTop) -> a 
  (IntVal i,IntVal j) -> IntVal $ i `intersect` j 
  (IntMemAddr m1,IntMemAddr m2) -> IntMemAddr $ m1 `meet_maddrs` m2 
  (InterVal (a,b),InterVal (c,d)) -> 
    let l = max a c
        u = min b d
    in if l > u
       then IntBot
       else InterVal (l,u)
  _ -> error $ "iMeet unsupported: " ++ show (v1,v2)

instance Num IntValue where
  (+) = plusValue
  (*) = multValue
  (-) = subValue
  negate = negateValue
  abs = error "abs for intervals unsupported" 
  signum = error "signum for intervals unsupported"
  fromInteger i = InterVal (I (fromInteger i), I (fromInteger i))

plusValue :: IntValue -> IntValue -> IntValue
plusValue v1 v2 = case (v1,v2) of
  (IntBot,_) -> IntBot
  (_,IntBot) -> IntBot
  (IntTop,_) -> IntTop
  (_,IntTop) -> IntTop
  (InterVal (a,b), InterVal (c,d)) -> InterVal (a+c, b+d)
  (IntVal i,IntVal j) -> IntVal [add_value a b | a <- i, b <- j]
  _ -> error "plusValue: non interval"

-- [a,b] * [c,d] = [minimum(ac,ad,bc,bd), maximum(ac,ad,bc,bd)]
multValue :: IntValue -> IntValue -> IntValue
multValue v1 v2 = case (v1,v2) of
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
    in InterVal (minimum list, maximum list) -- need to instantiate Eq, Ord
  (IntVal i,IntVal j) -> IntVal [mult_value a b | a <- i, b <- j]
  _ -> error "multValue: non interval"

subValue :: IntValue -> IntValue -> IntValue
subValue v1 v2 = case (v1,v2) of
  (IntBot,_) -> IntBot
  (_,IntBot) -> IntBot
  (IntTop,_) -> IntTop
  (_,IntTop) -> IntTop
  (InterVal (a,b), InterVal (c,d)) -> InterVal (a-d, b-c)
  (IntVal i,IntVal j) -> IntVal [sub_value a b | a <- i, b <- j]
  _ -> error "subValue: non interval"

negateValue :: IntValue -> IntValue
negateValue v = case v of
  IntBot -> IntBot
  IntTop -> IntTop
  InterVal (a,b) -> InterVal (negate a, negate b)
  IntVal i -> IntVal [minus_value a | a <- i]
  _ -> error "negateValue: non interval"

iDivide :: IntValue -> IntValue -> IntValue
iDivide v1 v2 = case (v1,v2) of
  (IntBot,_) -> IntBot
  (_,IntBot) -> IntBot
  (IntTop,_) -> IntTop
  (_,IntTop) -> IntTop
  (IntVal i,IntVal j) -> IntVal [div_value a b | a <- i, b <- j] 
  (ab,cd) -> 
          (v1 `interval_div` (v2 `iMeet` (i (I 1) PlusInf))) 
  `iJoin` 
          (v1 `interval_div` (v2 `iMeet` (i MinusInf (I (-1)))))

-- [a,b] / [c,d] = [a,b] * 1/[c,d]
interval_div :: IntValue -> IntValue -> IntValue
interval_div v1 v2 = case (v1,v2) of 
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

instance Hashable IntValue where
  hash v = case v of
    IntVal i -> hash i
    InterVal a -> hash a
    IntBot -> hash (0::Int)
    IntTop -> hash (1::Int)
    IntMemAddr mem -> hash mem 
    IntArr vals _ _ -> hash vals
  hashWithSalt s v = case v of
    IntVal i -> hashWithSalt s i
    InterVal a -> hashWithSalt s a
    IntBot -> hashWithSalt s (0::Int)
    IntTop -> hashWithSalt s (1::Int)
    IntMemAddr mem -> hashWithSalt s mem 
    IntArr vals _ _ -> hashWithSalt s vals
