-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Independence
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Domain.Concrete.Independence where

import qualified Data.Vector as V
import Model.GCS
import Model.Independence
import Language.SimpleC.AST (Statement)

-- read write data type
data RW = Read Variable | Write Variable
  deriving (Show,Eq,Ord)

type RWSet = [RW]

computeUIndep :: [(TransitionID, Statement, RWSet)] -> UIndep
computeUIndep rwsets = 
  let size = length rwsets
  in V.generate size (\i -> V.generate size (\j -> check rwsets i j))

check :: [(TransitionID, Statement, RWSet)] -> Int -> Int -> Bool
check rwsets i j = 
  let (_,_,tr1) = rwsets!!i
      (_,_,tr2) = rwsets!!j
  in not $ isRWDependent tr1 tr2

isRWDependent :: RWSet -> RWSet -> Bool
isRWDependent [] _ = False
isRWDependent ((Read v):rw1) rw2 = 
  any (\el -> el == Write v) rw2 || isRWDependent rw1 rw2
isRWDependent ((Write v):rw1) rw2 =
  any (\el -> el == Write v || el == Read v) rw2 || isRWDependent rw1 rw2
