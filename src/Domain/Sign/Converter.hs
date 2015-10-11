-------------------------------------------------------------------------------
-- Module    :  Domain.Sign.Converter
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Domain.Sign.Converter where

import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Util.Generic hiding (safeLookup)

