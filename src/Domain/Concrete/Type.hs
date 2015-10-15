{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Type
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Domain.Concrete.Type where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Model.GCS
import Util.Generic hiding (safeLookup)

type ISigma = Sigma
type Sigma = Map Var Value

instance Projection Sigma where
  controlPart =
    M.filterWithKey (\k _ -> isPC k)
  dataPart = 
    M.filterWithKey (\k _ -> not $ isPC k)
  subsumes a b = a == b
  
isPC :: Var -> Bool
isPC v = 
  let [_,x] = BSC.split '.' v
      x' = BSC.unpack x
  in x' == "pc"
  
data Value = 
      IntVal Int 
    | Array [Value]
  deriving (Show,Eq,Ord)

instance Hashable Sigma where
  hash = hash . M.toList
  hashWithSalt s st = hashWithSalt s $ M.toList st
  
instance Hashable Value where
  hash v = case v of
    IntVal i -> hash i
    Array vals -> hash vals
  hashWithSalt s v = case v of
    IntVal i -> hashWithSalt s i
    Array vals -> hashWithSalt s vals
      
toSigma :: LSigma -> Sigma
toSigma = M.fromList

insert :: Var -> Value -> Sigma -> Sigma
insert = M.insert

join :: Sigma -> Sigma -> Sigma
join = M.union

safeLookup :: String -> Sigma -> Var -> Value
safeLookup err st k =
  case M.lookup k st of 
    Nothing -> error $ "safeLookup: " ++ err
    Just v  -> v

-- Local State
type LSigma = [(Var,Value)]

showSigma :: Sigma -> String
showSigma s =
  let kv = M.toList s
  in showSigma' kv 

showSigma' :: [(Var, Value)] -> String
showSigma' [] = ""
showSigma' ((v,vs):rest) = show v ++ "=" ++ show vs ++ "\n" ++ showSigma' rest
{-      
   
-- Add a state to a list of states if that state is not already in the list
add :: Sigma s -> [Sigma s] -> ST s [Sigma s]
add s sts = do
    return $ s:sts
--  isEl <- isElem s sts
--  if isEl
--  then return sts
--  else return $ s:sts

-- This is the bottleneck
isElem :: Sigma -> [Sigma s] -> ST s Bool
isElem s [] = return False
isElem s (x:xs) = do
  isEq <- isEqual s x
  if isEq 
  then return True
  else isElem s xs

-- State equality: because I'm using a hashtable I need to stay within the ST monad
isEqual :: Sigma s -> Sigma s -> ST s Bool
isEqual s1 s2 = do
  l1 <- H.toList s1
  l2 <- H.toList s2
  return $ isEqual' l1 l2  

isEqual' :: [(Var,Value)] -> [(Var,Value)] -> Bool
isEqual' [] [] = True
isEqual' ((x,v):xs) ((y,t):ys) = x == y && v == t && isEqual' xs ys
-}
{-
sortSigmas :: [Sigma s] -> [Sigma s]
sortSigmas sts = 
  let sts' = map filterSigma sts
  in sort sts'

filterSigma :: Sigma s -> Sigma s
filterSigma st = M.filter (==1) st
-}