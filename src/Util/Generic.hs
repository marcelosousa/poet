module Util.Generic where

import Control.Applicative
import Control.Monad
import Control.Monad.ST.Safe
import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.HashTable.Class as H
import Data.List
import qualified Data.Maybe as M
import qualified Data.HashTable.ST.Cuckoo as C

type Var = BS.ByteString
type HashTable s k v = C.HashTable s k v

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f [] = return True
allM f (x:xs) = do
  b <- f x
  if b
  then allM f xs
  else return False 

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

third3 :: (a,b,c) -> c
third3 (a,b,c) = c

list :: b -> ([a] -> b) -> [a] -> b
list z f [] = z
list z f xs = f xs

catMaybes :: Eq a => [Maybe a] -> Maybe [a]
catMaybes [] = Nothing
catMaybes xs = Just $ nub $ M.catMaybes xs

safeLookup :: (Eq k, H.HashTable h, Hashable k) => String -> h s k b -> k -> ST s b
safeLookup err ht k = do
  mv <- H.lookup ht k
  case mv of 
    Nothing -> error $ "safeLookup: " ++ err
    Just v  -> return v

