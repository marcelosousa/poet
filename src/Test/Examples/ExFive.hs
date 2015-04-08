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
    (IntVal 1,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") (IntVal 2, Nothing)
      H.insert s (BS.pack "x") (IntVal 1, Nothing)
      return (s,[(BS.pack "pcp", IntVal 2),(BS.pack "x", IntVal 1)]) 
    _ -> return Nothing
t21_5' s = do
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      (IntVal lock,_) <- safeLookup "t21" s (BS.pack "lock")
      if lock == 0
      then do
        H.insert s (BS.pack "pcq") (IntVal 2, Nothing)
        H.insert s (BS.pack "lock") (IntVal 1, Nothing)
        return (s,[(BS.pack "pcq", IntVal 2),(BS.pack "lock", IntVal 1)]) 
      else do
        H.insert s (BS.pack "pcq") (IntVal 3, Nothing)
        return (s,[(BS.pack "pcq", IntVal 3)]) 
    _ -> return Nothing
t22_5' s = do
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      x <- safeLookup "t22" s (BS.pack "x")
      H.insert s (BS.pack "pcq") (IntVal 3, Nothing)
      H.insert s (BS.pack "x2") x
      return (s,[(BS.pack "pcq", IntVal 3),(BS.pack "x2", fst x)]) 
    _ -> return Nothing
t31_5' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      (IntVal lock,_) <- safeLookup "t31" s (BS.pack "lock")
      if lock == 0
      then do
        H.insert s (BS.pack "pcr") (IntVal 2, Nothing)
        H.insert s (BS.pack "lock") (IntVal 1, Nothing)
        return (s,[(BS.pack "pcr", IntVal 2),(BS.pack "lock", IntVal 1)]) 
      else do
        H.insert s (BS.pack "pcr") (IntVal 3, Nothing)
        return (s,[(BS.pack "pcr", IntVal 3)]) 
    _ -> return Nothing
t32_5' s = do
  v <- safeLookup "t32" s (BS.pack "pcr")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      x <- safeLookup "t32" s (BS.pack "x")
      H.insert s (BS.pack "pcr") (IntVal 3, Nothing)
      H.insert s (BS.pack "x3") x
      return (s,[(BS.pack "pcr", IntVal 3),(BS.pack "x3", fst x)]) 
    _ -> return Nothing


s5 :: ST s (Sigma s)
s5 = do 
  ht <- H.new
  H.insert ht (BS.pack "lock") (IntVal 0, Nothing)
  H.insert ht (BS.pack "x") (IntVal 0, Nothing)
  H.insert ht (BS.pack "x2") (IntVal 0, Nothing)
  H.insert ht (BS.pack "x3") (IntVal 0, Nothing) 
  H.insert ht (BS.pack "pcp") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "pcq") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "pcr") (IntVal 1, Nothing) 
  return ht

t1_5, t21_5,t22_5,t31_5,t32_5 :: Transition s
t1_5  = (BS.pack "p", 0, t1_5')
t21_5 = (BS.pack "q", 1, t21_5')
t22_5 = (BS.pack "q", 2, t22_5')
t31_5 = (BS.pack "r", 3, t31_5')
t32_5 = (BS.pack "r", 4, t32_5')

sys5 :: ST s (System s)
sys5 = do 
  is <- s5
  lis <- H.toList is >>= return . map (\(a,b) -> (a, fst b)) 
  return $ System (V.fromList [t1_5,t21_5,t22_5,t31_5,t32_5]) is lis

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
