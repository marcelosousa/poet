module Test.Examples.ExOne where

import Model.GCS

import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V

-- Example 1 - Two writes of different variables
-- Sigma s -> ST s (Maybe (Sigma s -> ST s (Sigma s)))
t1' :: TransitionFn s 
t1' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    (IntVal 1, _) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") (IntVal 2, Nothing)
      H.insert s (BS.pack "x") (IntVal 1, Nothing)
      return (s, [(BS.pack "pcp", (IntVal 2, Nothing)),(BS.pack "x", (IntVal 1, Nothing))]) 
    _ -> return Nothing

t1 :: Transition s
t1 = (BS.pack "p", 0, t1')


t2' :: TransitionFn s 
t2' s = do
  v <- safeLookup "t2" s (BS.pack "pcq")
  case v of
    (IntVal 1, _) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") (IntVal 2, Nothing)
      H.insert s (BS.pack "y") (IntVal 1, Nothing)
      return (s, [(BS.pack "pcq", (IntVal 2, Nothing)),(BS.pack "y", (IntVal 1, Nothing))]) 
    _ -> return Nothing

t2 :: Transition s
t2 = (BS.pack "q", 1, t2')

s1 :: ST s (Sigma s)
s1 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "pcq") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "x") (IntVal 0, Nothing) 
  H.insert ht (BS.pack "y") (IntVal 0, Nothing) 
  return ht

sys1 :: ST s (System s)
sys1 = do 
  is <- s1
  lis <- H.toList is
  return $ System (V.fromList [t1,t2]) is lis

ind11,ind12 :: UIndep
ind11 = V.generate 2 (\i -> V.generate 2 (\j -> False)) 
ind12 = V.generate 2 (\i -> V.generate 2 (\j -> if i /= j then True else False)) 

