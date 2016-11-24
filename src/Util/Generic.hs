module Util.Generic where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.HashTable.Class as H
import Data.List
import qualified Data.Maybe as M
import qualified Data.HashTable.ST.Cuckoo as C
import Language.SimpleC.AST (SymId)
import qualified Debug.Trace as T

mytrace True a b = T.trace a b
mytrace False a b = b

type Var = SymId -- BS.ByteString
type HashTable s k v = C.HashTable s k v

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f [] = return True
allM f (x:xs) = do
  b <- f x
  if b
  then allM f xs
  else return False 

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

snd4 :: (a,b,c,d) -> b
snd4 (a,b,c,d) = b

trd4 :: (a,b,c,d) -> c
trd4 (a,b,c,d) = c

frd4 :: (a,b,c,d) -> d
frd4 (a,b,c,d) = d

list :: b -> ([a] -> b) -> [a] -> b
list z f [] = z
list z f xs = f xs

catMaybes :: Eq a => [Maybe a] -> Maybe [a]
catMaybes [] = Nothing
catMaybes [ma] =
  case ma of
      Nothing -> Nothing
      Just x  -> Just [x]
catMaybes (ma:rest) =
    case ma of
        Nothing -> catMaybes rest
        Just x -> case catMaybes rest of
            Nothing -> Just [x]
            Just r -> Just $ nub $ x:r

--catMaybes :: Eq a => [Maybe a] -> Maybe [a]
--catMaybes [] = Nothing
--catMaybes xs = Just $ nub $ M.catMaybes xs

safeLookup :: (Eq k, H.HashTable h, Hashable k) => String -> h s k b -> k -> ST s b
safeLookup err ht k = do
  mv <- H.lookup ht k
  case mv of 
    Nothing -> error $ "safeLookup: " ++ err
    Just v  -> return v

toBool :: Int -> Bool
toBool 0 = False
toBool 1 = True
toBool x = error $ "toBool: " ++ show x

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1
