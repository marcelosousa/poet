module Test.Examples.ExFour where

import Model.GCS

import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V

-- Example 4
s4 :: ST s (Sigma s)
s4 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") (IntVal 1, Nothing)
  H.insert ht (BS.pack "pcq") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "x") (IntVal 0, Nothing) 
  H.insert ht (BS.pack "y") (IntVal 0, Nothing) 
  H.insert ht (BS.pack "z") (IntVal 0, Nothing) 
  return ht

t11_4, t12_4, t21_4, t22_4 :: Transition s
t11_4 = (BS.pack "p",0,t11_4')
t12_4 = (BS.pack "p",1,t12_4')
t21_4 = (BS.pack "q",2,t21_4')
t22_4 = (BS.pack "q",3,t22_4')

t11_4', t12_4', t21_4', t22_4' :: TransitionFn s 
t11_4' s = do
  v <- safeLookup "t11" s (BS.pack "pcp")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") (IntVal 2, Nothing)
      H.insert s (BS.pack "y") (IntVal 1, Nothing)
      return (s,[(BS.pack "pcp", IntVal 2),(BS.pack "y", IntVal 1)]) 
    _ -> return Nothing
t12_4' s = do 
  v <- safeLookup "t12" s (BS.pack "pcp")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") (IntVal 3, Nothing)
      H.insert s (BS.pack "x") (IntVal 1, Nothing)
      return (s,[(BS.pack "pcp", IntVal 3),(BS.pack "x", IntVal 1)]) 
    _ -> return Nothing
t21_4' s = do 
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") (IntVal 2, Nothing)
      H.insert s (BS.pack "z") (IntVal 1, Nothing)
      return (s,[(BS.pack "pcq", IntVal 2),(BS.pack "z", IntVal 1)]) 
    _ -> return Nothing
t22_4' s = do 
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") (IntVal 3, Nothing)
      H.insert s (BS.pack "x") (IntVal 2, Nothing)
      return (s,[(BS.pack "pcq", IntVal 3),(BS.pack "x", IntVal 2)]) 
    _ -> return Nothing

sys4 :: ST s (System s)
sys4 = do 
  is <- s4
  lis <- H.toList is >>= return . map (\(a,b) -> (a, fst b)) 
  return $ System (V.fromList [t11_4,t12_4,t21_4,t22_4]) is lis

ind4 :: UIndep
ind4 = V.generate 4 (\i -> V.generate 4 (\j -> check4 i j)) 

-- ind4 = [("t1","t3"),("t1","t4"),("t2","t3")]
check4 :: Int -> Int -> Bool
check4 0 2 = True
check4 2 0 = True
check4 0 3 = True
check4 3 0 = True
check4 1 2 = True
check4 2 1 = True
check4 _ _ = False