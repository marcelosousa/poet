module Test.Examples.ExTen (sys10, ind10) where

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

t21' :: TransitionFn s
t21' s = do
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      H.insert s (BS.pack "x") (IntVal 2)
      H.insert s (BS.pack "pcq") (IntVal 2)
      return s
    _ -> return Nothing

t21 :: Transition s
t21 = (BS.pack "q", 1, [Other], t21')

t31' :: TransitionFn s
t31' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      H.insert s (BS.pack "y") (IntVal 1)
      H.insert s (BS.pack "pcr") (IntVal 2)
      return s
    _ -> return Nothing

t31 :: Transition s
t31 = (BS.pack "r", 2, [Other], t31')

t41' :: TransitionFn s
t41' s = do
  v <- safeLookup "t41" s (BS.pack "pcs") 
  case v of 
    (IntVal 1) ->  return $ Just $ \s -> do
      H.insert s (BS.pack "y") (IntVal 2)
      H.insert s (BS.pack "pcs") (IntVal 2)
      return s
    _ -> return Nothing

t41 :: Transition s
t41 = (BS.pack "s", 3, [Other], t41')

s :: ST s (Sigma s)  
s = do 
  ht <- H.new
  H.insert ht (BS.pack "x") (IntVal 0)
  H.insert ht (BS.pack "y") (IntVal 0)
  H.insert ht (BS.pack "pcp") (IntVal 1)
  H.insert ht (BS.pack "pcq") (IntVal 1)
  H.insert ht (BS.pack "pcr") (IntVal 1)
  H.insert ht (BS.pack "pcs") (IntVal 1)      
  return ht
  
sys10 :: ST s (System s)
sys10 = do 
  is <- s
  return $ System (V.fromList [t11,t21,t31,t41]) is [Other]

ind10 :: UIndep
ind10 = V.generate 4 (\i -> V.generate 4 (\j -> check i j))

check :: Int -> Int -> Bool
check 0 2 = True
check 2 0 = True
check 0 3 = True
check 3 0 = True
check 1 2 = True
check 2 1 = True
check 1 3 = True
check 3 1 = True
check _ _ = False
