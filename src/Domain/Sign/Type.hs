-------------------------------------------------------------------------------
-- Module    :  Domain.Sign.Type
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Domain.Sign.Type where

import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Util.Generic hiding (safeLookup)

import Domain.Domain

data Sign = Bot | Negative | Zero | Positive | Top
    deriving (Show, Eq, Ord)

instance Domain Sign where
  join Bot a = a
  join a Bot = a
  join a b = if a == b then a else Top
  meet Top a = a
  meet a Top = a
  meet a b = if a == b then a else Bot

--instance Hashable Sign where
--  hash = hash . M.toList
--  hashWithSalt s st = hashWithSalt s $ M.toList st
