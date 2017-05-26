{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Value
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- The value for the concrete semantics.
-------------------------------------------------------------------------------
module Domain.Concrete.Value where

import Data.Hashable
import Data.List
import Domain.Util
import Domain.Action
import Domain.MemAddr
import Language.SimpleC.Util
import Util.Generic hiding (safeLookup)
import qualified Data.Set as S

-- | Concrete Action
type ConAct = Act ConValue

-- | Concrete Memory Addr
type ConMAddr  = MemAddr  ConValue
type ConMAddrs = MemAddrs ConValue

type ConOffsList = [(ConValue, ConValue)]

type ConValues = [ConValue]

-- | Concrete Value for the concrete semantics
data ConValue
  =  ConVal Value      -- Concrete list of values
  -- Memory address value: address
  | ConMemAddr ConMAddrs
  -- Array value
  -- Memory address for the positions and the size
  | ConArr ConValues Int Bool -- IsTop 
  | ConBot
  deriving (Show,Eq,Ord)

checkBoolVals :: ConValues -> (Bool,Bool)
checkBoolVals vals = (any isTrue vals, any isFalse vals)

isTrue :: ConValue -> Bool
isTrue val = case val of
  ConVal v -> case v of
    VBool b -> b
    _ -> False 
  _ -> False  

isFalse :: ConValue -> Bool
isFalse val = case val of
  ConVal v -> case v of
    VBool b -> not b
    _ -> False 
  _ -> False 

k :: Int -> ConValue
k = ConVal . VInt

zero :: ConValue
zero = k 0

one :: ConValue
one = k 1

instance Num ConValue where
  (+) c1 c2 = case (c1,c2) of
    (ConVal v1,ConVal v2) -> ConVal $ add_value v1 v2
    _  -> error "add_conval: not ConVal" 
  (*) c1 c2 = case (c1,c2) of
    (ConVal v1,ConVal v2) -> ConVal $ mult_value v1 v2
    _  -> error "mult_ConVal: not ConVal"
  (-) c1 c2 = case (c1,c2) of
    (ConVal v1,ConVal v2) -> ConVal $ sub_value v1 v2
    _  -> error "sub_ConVal: not ConVal"
  negate c1 = 
    case c1 of
      ConVal v -> ConVal $ neg_value v
  abs           = error "abs         for ConVal unsupported" 
  signum        = error "signum      for ConVal unsupported"
  fromInteger i = error "fromInteger for ConVal unsupported"
  
divs_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ div_value v1 v2
  _  -> error "div_ConVal: not ConVal"
  
rmdr_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ rmd_value v1 v2
  _  -> error "rmdr_ConVal: not ConVal"
  
minus_conval c1 = case c1 of
  ConVal v1 -> ConVal $ minus_value v1
  _ -> error "minus_conval: not ConVal"

-- Boolean operations in ConValue
le_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ le_value v1 v2
  _ -> error "le_conval: not conVal" 
gr_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ gr_value v1 v2
  _ -> error "gr_conval: not conVal"
leq_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ lor_value (gr_value v1 v2) (eq_value v1 v2) 
  _ -> error "leq_conval: not conVal"
geq_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ lor_value (gr_value v1 v2) (eq_value v1 v2) 
  _ -> error "geq_conval: not conVal"
eq_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ eq_value v1 v2 
  _ -> error "eq_conval: not conVal"
neq_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ neg_value $ eq_value v1 v2 
  _ -> error "neq_conval: not conVal"
land_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ land_value v1 v2 
  _ -> error "neq_conval: not conVal"
lor_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ lor_value v1 v2 
  _ -> error "neq_conval: not conVal"
   
instance Hashable ConValue where
  hash v = case v of
    ConVal val -> hash val
    ConMemAddr mem -> hash mem
    _ -> error "hash not supported" 
  hashWithSalt s v = case v of
    ConVal val -> hashWithSalt s val
    ConMemAddr mem -> hashWithSalt s mem
    _ -> error "hash not supported" 