module Test.Examples.ExFive where

import Model.GCS

import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V

-- Example 5 - Cesar's example
t1_5',t21_5',t22_5',t31_5',t32_5' :: TransitionFn s
t1_5' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      let pcVal = (IntVal 2)
          xVal = (IntVal 1)
      H.insert s (BS.pack "pcp") pcVal
      H.insert s (BS.pack "x") xVal
      return s
    _ -> return Nothing
t21_5' s = do
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      (IntVal lock) <- safeLookup "t21" s (BS.pack "lock")
      if lock == 0
      then do
        let pcVal = (IntVal 2)
            lockVal = (IntVal 1)
        H.insert s (BS.pack "pcq") pcVal
        H.insert s (BS.pack "lock") lockVal
        return s
      else do
        let pcVal = (IntVal 3)
        H.insert s (BS.pack "pcq") pcVal
        return s
    _ -> return Nothing
t22_5' s = do
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
    (IntVal 2) -> return $ Just $ \s -> do
      x <- safeLookup "t22" s (BS.pack "x")
      let pcVal = (IntVal 3)
      H.insert s (BS.pack "pcq") pcVal
      H.insert s (BS.pack "x2") x
      return s
    _ -> return Nothing
t31_5' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      (IntVal lock) <- safeLookup "t31" s (BS.pack "lock")
      if lock == 0
      then do
        let pcVal = (IntVal 2)
            lockVal = (IntVal 1)
        H.insert s (BS.pack "pcr") pcVal
        H.insert s (BS.pack "lock") lockVal
        return s
      else do
        let pcVal = (IntVal 3)
        H.insert s (BS.pack "pcr") pcVal
        return s
    _ -> return Nothing
t32_5' s = do
  v <- safeLookup "t32" s (BS.pack "pcr")
  case v of
    (IntVal 2) -> return $ Just $ \s -> do
      x <- safeLookup "t32" s (BS.pack "x")
      let pcVal = (IntVal 3)
      H.insert s (BS.pack "pcr") pcVal
      H.insert s (BS.pack "x3") x
      return s
    _ -> return Nothing


s5 :: ST s (Sigma s)
s5 = do 
  ht <- H.new
  H.insert ht (BS.pack "lock") (IntVal 0)
  H.insert ht (BS.pack "x") (IntVal 0)
  H.insert ht (BS.pack "x2") (IntVal 0)
  H.insert ht (BS.pack "x3") (IntVal 0) 
  H.insert ht (BS.pack "pcp") (IntVal 1) 
  H.insert ht (BS.pack "pcq") (IntVal 1) 
  H.insert ht (BS.pack "pcr") (IntVal 1) 
  return ht

t1_5, t21_5,t22_5,t31_5,t32_5 :: Transition s
t1_5  = (BS.pack "p", 0, [Other], t1_5')
t21_5 = (BS.pack "q", 1, [Other], t21_5')
t22_5 = (BS.pack "q", 2, [Other], t22_5')
t31_5 = (BS.pack "r", 3, [Other], t31_5')
t32_5 = (BS.pack "r", 4, [Other], t32_5')

sys5 :: ST s (System s)
sys5 = do 
  is <- s5
  return $ System (V.fromList [t1_5,t21_5,t22_5,t31_5,t32_5]) is [Other]

-- [("t1","t2"),("t1","t4"),("t3","t5"),("t2","t5"),("t3","t4")]
ind5 :: UIndep
ind5 = V.generate 5 (\i -> V.generate 5 (\j -> check5 i j)) 

check5 :: Int -> Int -> Bool
check5 0 1 = True
check5 1 0 = True
check5 0 3 = True
check5 3 0 = True
check5 2 4 = True
check5 4 2 = True
check5 1 4 = True
check5 4 1 = True
check5 2 3 = True
check5 3 2 = True
check5 _ _ = False
