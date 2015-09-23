module Test.Examples.ExEight (sys8, ind8) where

import Domain.Concrete
import Model.GCS
import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V
import Util.Generic

-- Example 8 - Lock example
t11',t12',t21',t22',t23' :: TransitionFn s
t11' s = do
  v <- safeLookup "t11" s (BS.pack "pcq")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      let pcVal = (IntVal 2)
      H.insert s (BS.pack "pcq") pcVal
      H.insert s (BS.pack "lock") (IntVal 0)
      return s
    _ -> return Nothing

t12' s = do
  v <- safeLookup "t12" s (BS.pack "pcq")
  case v of
    (IntVal 2) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") (IntVal 3)
      H.insert s (BS.pack "x") (IntVal 1)
      return s
    _ -> return Nothing

t21' s = do
  (IntVal v) <- safeLookup "t21" s (BS.pack "pcr")
  (IntVal lock) <- safeLookup "t21" s (BS.pack "lock")
  if v == 1 && lock == 0
  then return $ Just $ \s -> do
    let pcVal = (IntVal 2)
        lockVal = (IntVal 1)
    H.insert s (BS.pack "pcr") pcVal
    H.insert s (BS.pack "lock") lockVal
    return s
  else return Nothing
t22' s = do
  v <- safeLookup "t22" s (BS.pack "pcr")
  case v of
    (IntVal 2) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcr") (IntVal 3)
      H.insert s (BS.pack "x") (IntVal 2)
      return s
    _ -> return Nothing
t23' s = do
  v <- safeLookup "t23" s (BS.pack "pcr")
  case v of
    (IntVal 3) -> return $ Just $ \s -> do
      let pcVal = (IntVal 4)
      H.insert s (BS.pack "pcr") pcVal
      H.insert s (BS.pack "lock") (IntVal 0)
      return s
    _ -> return Nothing

s7 :: ST s (Sigma s)
s7 = do 
  ht <- H.new
  H.insert ht (BS.pack "lock") (IntVal 1)
  H.insert ht (BS.pack "x") (IntVal 0)
  H.insert ht (BS.pack "pcq") (IntVal 1) 
  H.insert ht (BS.pack "pcr") (IntVal 1) 
  return ht

t11,t12,t21,t22,t23 :: Transition s
t11 = (BS.pack "q", 0, [Unlock $ V $ BS.pack "lock"], t11')
t12 = (BS.pack "q", 1, [Other], t12')
t21 = (BS.pack "r", 2, [Lock $ V $ BS.pack "lock"], t21')
t22 = (BS.pack "r", 3, [Other], t22')
t23 = (BS.pack "r", 4, [Unlock $ V  $ BS.pack "lock"], t23')

sys8 :: ST s (System s)
sys8 = do 
  is <- s7
  return $ System (V.fromList [t11,t12,t21,t22,t23]) is [Lock $ V $ BS.pack "lock"]

ind8 :: UIndep
ind8 = V.generate 5 (\i -> V.generate 5 (\j -> check i j)) 

check :: Int -> Int -> Bool
check 0 3 = True
check 3 0 = True
check 1 2 = True
check 2 1 = True
check 1 4 = True
check 4 1 = True
check _ _ = False
