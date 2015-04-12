module Test.Examples.ExNine (sys9, ind9) where

import Model.GCS

import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V

t11' :: TransitionFn s
t11' s = do
  return $ Just $ \s -> do
    H.insert s (BS.pack "x") (IntVal 1)
    return s

t11 :: Transition s
t11 = (BS.pack "m", 0, [Other], t11')

s :: ST s (Sigma s)  
s = do 
  ht <- H.new
  H.insert ht (BS.pack "x") (IntVal 0)
  return ht
  
sys9 :: ST s (System s)
sys9 = do 
  is <- s
  return $ System (V.fromList [t11]) is [Other]

ind9 :: UIndep
ind9 = V.generate 1 (\i -> V.generate 1 (\j -> False)) 
