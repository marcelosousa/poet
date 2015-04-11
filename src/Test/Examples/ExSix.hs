module Test.Examples.ExSix where

import Model.GCS

import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V


-- Example 6
s6 :: ST s (Sigma s)
s6 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") (IntVal 1) 
  H.insert ht (BS.pack "pcq") (IntVal 1) 
  H.insert ht (BS.pack "pcr") (IntVal 1)
  H.insert ht (BS.pack "x") (IntVal 1) 
  H.insert ht (BS.pack "x1") (IntVal 0) 
  H.insert ht (BS.pack "x2") (IntVal 0)  
  return ht

t1_6, t21_6, t22_6, t31_6, t32_6 :: Transition s
t1_6 = (BS.pack "p",0, [Other],t1_6')
t21_6 = (BS.pack "q",1,[Other],t21_6')
t22_6 = (BS.pack "q",2,[Other],t22_6')
t31_6 = (BS.pack "r",3,[Other],t31_6')
t32_6 = (BS.pack "r",4,[Other],t32_6')

t1_6', t21_6', t22_6', t31_6', t32_6' :: TransitionFn s
t1_6' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      x <- safeLookup "t1" s (BS.pack "x")
      let pcVal = (IntVal 2) 
      H.insert s (BS.pack "pcp") pcVal
      H.insert s (BS.pack "x1") x
      return s
    _ -> return Nothing
t21_6' s = do
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      x <- safeLookup "t1" s (BS.pack "x")
      let pcVal = (IntVal 2)
      H.insert s (BS.pack "pcq") pcVal
      H.insert s (BS.pack "x2") x
      return s
    _ -> return Nothing
t22_6' s = do 
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
    (IntVal 2) -> return $ Just $ \s -> do
      let pcVal = (IntVal 3) 
      H.insert s (BS.pack "pcq") pcVal
      return s
    _ -> return Nothing
t31_6' s = do 
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      let pcVal = (IntVal 2) 
      H.insert s (BS.pack "pcr") pcVal
      return s
    _ -> return Nothing
t32_6' s = do
  v <- safeLookup "t32" s (BS.pack "pcr")
  case v of
    (IntVal 2) -> return $ Just $ \s -> do
      let pcVal = (IntVal 3) 
          xVal = (IntVal 2)
      H.insert s (BS.pack "pcr") pcVal
      H.insert s (BS.pack "x") xVal
      return s
    _ -> return Nothing

sys6 :: ST s (System s)
sys6 = do 
  is <- s6
  return $ System (V.fromList [t1_6,t21_6,t22_6,t31_6,t32_6]) is [Other]

ind6 :: UIndep
ind6 = V.generate 5 (\i -> V.generate 5 (\j -> check6 i j)) 

check6 :: Int -> Int -> Bool
-- R R
check6 0 1 = True
check6 1 0 = True
-- R K
check6 0 2 = True
check6 2 0 = True
-- R K
check6 0 3 = True
check6 3 0 = True
-- R K
check6 1 3 = True
check6 3 1 = True
-- K W
check6 2 4 = True
check6 4 2 = True
-- R K
check6 2 3 = True
check6 3 2 = True
check6 _ _ = False