module Test.Examples.ExEleven (sys11, ind11) where

import Domain.Concrete
import Model.GCS

import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V
import Util.Generic
t11' :: TransitionFn s
t11' s = do
  v <- safeLookup "t11" s (BS.pack "pcp")
  case v of
      (IntVal 1) -> return $ Just $ \s -> do
        H.insert s (BS.pack "x") (IntVal 1)
        H.insert s (BS.pack "pcp") (IntVal 2)
        return s
      _ -> return Nothing

t11 :: Transition s
t11 = (BS.pack "p", 0, [Other], t11')

t12' :: TransitionFn s
t12' s = do
  v <- safeLookup "t12" s (BS.pack "pcp")
  case v of
      (IntVal 2) -> return $ Just $ \s -> do
        H.insert s (BS.pack "y") (IntVal 1)
        H.insert s (BS.pack "pcp") (IntVal 3)
        return s
      _ -> return Nothing

t12 :: Transition s
t12 = (BS.pack "p", 1, [Other], t12')

t21' :: TransitionFn s
t21' s = do
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
      (IntVal 1) -> return $ Just $ \s -> do
        H.insert s (BS.pack "y") (IntVal 2)
        H.insert s (BS.pack "pcq") (IntVal 2)
        return s
      _ -> return Nothing

t21 :: Transition s
t21 = (BS.pack "q", 2, [Other], t21')

t22' :: TransitionFn s
t22' s = do
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
      (IntVal 2) -> return $ Just $ \s -> do
        H.insert s (BS.pack "x") (IntVal 2)
        H.insert s (BS.pack "pcq") (IntVal 3)
        return s
      _ -> return Nothing

t22 :: Transition s
t22 = (BS.pack "q", 3, [Other], t22')

s :: ST s (Sigma s)  
s = do 
  ht <- H.new
  H.insert ht (BS.pack "x") (IntVal 0)
  H.insert ht (BS.pack "y") (IntVal 0)
  H.insert ht (BS.pack "pcp") (IntVal 1)
  H.insert ht (BS.pack "pcq") (IntVal 1)
  return ht
  
sys11 :: ST s (System s)
sys11 = do 
  is <- s
  return $ System (V.fromList [t11,t12,t21,t22]) is [Other]

ind11 :: UIndep
ind11 = V.generate 4 (\i -> V.generate 4 (\j -> check i j))

check :: Int -> Int -> Bool
check 0 2 = True
check 2 0 = True
check 1 3 = True
check 3 1 = True
check _ _ = False
