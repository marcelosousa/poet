{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Offset
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- The offsets for the interval semantics.
-------------------------------------------------------------------------------
module Domain.Interval.Offset where

import Data.Hashable
import Data.List
import Domain.Action
import Domain.Interval.Type
import Domain.Interval.Value
import Domain.MemAddr
import Domain.Lattice
import Domain.Util
import Language.SimpleC.Util
import Util.Generic hiding (safeLookup)
import qualified Data.Set as S

-- | Interval offset list
type IntOffsList = [(IntValue, IntValue)]

-- Functions over two lists of interval offsets
--  Subsumption
--  Join
--  Widening
-- X1 >= X2

instance {-# OVERLAPS #-} Ord IntOffsList where
   (<=) a b = b `subsumes_intval_list` a
   
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
                     
-- Generic function that matches 
-- the offset intervals and applies a function over its values
gen_intval_list :: (IntValue -> IntValue -> IntValue) -> IntOffsList -> IntOffsList -> IntOffsList
gen_intval_list fn _x1 _x2 = 
  case (_x1, _x2) of
    ([], _) -> _x2 
    (_, []) -> _x1
    ((a,v):x1, (b,u):x2) -> mytrace False ("gen_intval_list: " ++ show (a,b)) $
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
                in (b,v `fn` u):(gen_intval_list fn ((a',v):x1) x2)
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
                LT -> (b,u):(gen_intval_list fn _x1 x2)
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

instance Lattice IntOffsList where
   bot   = []
   top   = error "top for IntOffsList"
   join  = gen_intval_list join 
   meet  = gen_intval_list meet 
   widen = gen_intval_list widen 
   