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
  H.insert ht (BS.pack "pcp") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "pcq") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "pcr") (IntVal 1, Nothing)
  H.insert ht (BS.pack "x") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "x1") (IntVal 0, Nothing) 
  H.insert ht (BS.pack "x2") (IntVal 0, Nothing)  
  return ht

t1_6, t21_6, t22_6, t31_6, t32_6 :: Transition s
t1_6 = (BS.pack "p",0,t1_6')
t21_6 = (BS.pack "q",1,t21_6')
t22_6 = (BS.pack "q",2,t22_6')
t31_6 = (BS.pack "r",3,t31_6')
t32_6 = (BS.pack "r",4,t32_6')

t1_6', t21_6', t22_6', t31_6', t32_6' :: TransitionFn s
t1_6' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    (IntVal 1, _) -> return $ Just $ \s -> do
      x <- safeLookup "t1" s (BS.pack "x")
      H.insert s (BS.pack "pcp") (IntVal 2, Nothing) 
      H.insert s (BS.pack "x1") x
      return (s,[(BS.pack "pcp", IntVal 2),(BS.pack "x1", fst x)]) 
    _ -> return Nothing
t21_6' s = do
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    (IntVal 1, _) -> return $ Just $ \s -> do
      x <- safeLookup "t1" s (BS.pack "x")
      H.insert s (BS.pack "pcq") (IntVal 2, Nothing) 
      H.insert s (BS.pack "x2") x
      return (s,[(BS.pack "pcq", IntVal 2),(BS.pack "x2", fst x)]) 
    _ -> return Nothing
t22_6' s = do 
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
    (IntVal 2, _) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") (IntVal 3, Nothing) 
      return (s,[(BS.pack "pcq", IntVal 3)]) 
    _ -> return Nothing
t31_6' s = do 
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcr") (IntVal 2, Nothing) 
      return (s,[(BS.pack "pcr", IntVal 2)]) 
    _ -> return Nothing
t32_6' s = do
  v <- safeLookup "t32" s (BS.pack "pcr")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcr") (IntVal 3, Nothing) 
      H.insert s (BS.pack "x") (IntVal 2, Nothing) 
      return (s,[(BS.pack "pcr", IntVal 3),(BS.pack "x", IntVal 2)]) 
    _ -> return Nothing

sys6 :: ST s (System s)
sys6 = do 
  is <- s6
  lis <- H.toList is >>= return . map (\(a,b) -> (a, fst b))
  return $ System (V.fromList [t1_6,t21_6,t22_6,t31_6,t32_6]) is lis

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