-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Domain.Concrete where

import Control.Monad.ST.Safe
import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.List

type ISigma s = Sigma s
-- A state is an Hash Table
type HashTable s k v = C.HashTable s k v
--  Later we can try Judy Arrays or Mutable Vectors
type Sigma s = HashTable s Var Value -- We may want to add the enabled transitions for efficiency.
type SigmaRaw = [(Var,Value)]
type Var = BS.ByteString
data Value = 
      IntVal Int 
    | Array [Value]
  deriving (Show,Eq,Ord)

instance Hashable Value where
    hash v = case v of
      IntVal i -> hash i
      Array vals -> hash vals
    hashWithSalt s v = case v of
      IntVal i -> hashWithSalt s i
      Array vals -> hashWithSalt s vals
      
-- Local State
type LSigma = [(Var,Value)]

-- This needs to be more efficient
copy :: Sigma s -> ST s (Sigma s)
copy s = do 
  kv <- H.toList s
  H.fromList kv 

showSigma :: Sigma s -> ST s String
showSigma s = do
  kv <- H.toList s
  return $ showSigma' kv 

showSigma' :: [(Var, Value)] -> String
showSigma' [] = ""
showSigma' ((v,vs):rest) = show v ++ "=" ++ show vs ++ "\n" ++ showSigma' rest
      
equals :: Sigma s -> Sigma s -> ST s Bool
equals s1 s2 = do
  kv1 <- H.toList s1
  kv2 <- H.toList s2
  return $ sort kv1 == sort kv2

   
-- Add a state to a list of states if that state is not already in the list
add :: Sigma s -> [Sigma s] -> ST s [Sigma s]
add s sts = do
    return $ s:sts
--  isEl <- isElem s sts
--  if isEl
--  then return sts
--  else return $ s:sts

-- This is the bottleneck
isElem :: Sigma s -> [Sigma s] -> ST s Bool
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

{-
sortSigmas :: [Sigma s] -> [Sigma s]
sortSigmas sts = 
  let sts' = map filterSigma sts
  in sort sts'

filterSigma :: Sigma s -> Sigma s
filterSigma st = M.filter (==1) st
-}