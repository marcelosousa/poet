module Test.Examples.ExTwo where

import Domain.Concrete
import Model.GCS

import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V
import Util.Generic
-- Example 2 - 1 write, 2 reads
s2 :: ST s (Sigma s)
s2 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") (IntVal 1)
  H.insert ht (BS.pack "pcq") (IntVal 1) 
  H.insert ht (BS.pack "pcr") (IntVal 1)
  H.insert ht (BS.pack "x")   (IntVal 0)
  H.insert ht (BS.pack "l1")  (IntVal 0)
  H.insert ht (BS.pack "l2")  (IntVal 0)
  return ht

t1_2, t2_2, t3_2 :: Transition s
t1_2 = (BS.pack "p",0,[Other],t1_2')
t2_2 = (BS.pack "q",1,[Other],t2_2')
t3_2 = (BS.pack "r",2,[Other],t3_2')

t1_2', t2_2', t3_2' :: TransitionFn s 
t1_2' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    IntVal 1 -> return $ Just $ \s -> do
      let pcVal = (IntVal 2)
          xVal = (IntVal 1)
      H.insert s (BS.pack "pcp") pcVal
      H.insert s (BS.pack "x") xVal
      return s
    _ -> return Nothing
t2_2' s = do
  v <- safeLookup "t2" s (BS.pack "pcq")
  case v of
    IntVal 1 -> return $ Just $ \s -> do
      let pcVal = (IntVal 2)
      H.insert s (BS.pack "pcq") pcVal
      x <- safeLookup "t2" s (BS.pack "x")
      H.insert s (BS.pack "l1") x
      return s
    _ -> return Nothing
t3_2' s = do
  v <- safeLookup "t3" s (BS.pack "pcr")
  case v of
    IntVal 1 -> return $ Just $ \s -> do
      let pcVal = (IntVal 2)
      H.insert s (BS.pack "pcr") pcVal
      x <- safeLookup "t3" s (BS.pack "x")
      H.insert s (BS.pack "l2") x
      return s
    _ -> return Nothing

sys2 :: ST s (System s)
sys2 = do 
  is <- s2
  return $ System (V.fromList [t1_2,t2_2,t3_2]) is [Other]

ind2 :: UIndep
ind2 = V.generate 3 (\i -> V.generate 3 (\j -> check2 i j)) 
--ind2 = V.generate 3 (\i -> V.generate 3 (\j -> False)) 

check2 :: Int -> Int -> Bool
check2 1 2 = True
check2 2 1 = True
check2 _ _ = False