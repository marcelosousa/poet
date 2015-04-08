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
      let pcVal = (IntVal 2, Nothing)
          yVal = (IntVal 1, Nothing)
      H.insert s (BS.pack "pcp") pcVal      
      H.insert s (BS.pack "y") yVal
      return (s,[(BS.pack "pcp", pcVal),(BS.pack "y", yVal)]) 
    _ -> return Nothing
t12_4' s = do 
  v <- safeLookup "t12" s (BS.pack "pcp")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      let pcVal = (IntVal 3, Nothing)
          xVal = (IntVal 1, Nothing)
      H.insert s (BS.pack "pcp") pcVal
      H.insert s (BS.pack "x") xVal
      return (s,[(BS.pack "pcp", pcVal),(BS.pack "x", xVal)]) 
    _ -> return Nothing
t21_4' s = do 
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      let pcVal = (IntVal 2, Nothing)
          zVal = (IntVal 1, Nothing)
      H.insert s (BS.pack "pcq") pcVal
      H.insert s (BS.pack "z") zVal
      return (s,[(BS.pack "pcq", pcVal),(BS.pack "z", zVal)]) 
    _ -> return Nothing
t22_4' s = do 
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      let pcVal = (IntVal 3, Nothing)
          xVal = (IntVal 2, Nothing)
      H.insert s (BS.pack "pcq") pcVal
      H.insert s (BS.pack "x") xVal
      return (s,[(BS.pack "pcq", pcVal),(BS.pack "x", xVal)]) 
    _ -> return Nothing

sys4 :: ST s (System s)
sys4 = do 
  is <- s4
  lis <- H.toList is
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