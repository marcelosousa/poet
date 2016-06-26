{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Type
-- Copyright :  (c) 2015-16 Marcelo Sousa
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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Model.GCS
import Util.Generic hiding (safeLookup)
import Language.SimpleC.AST
import Language.SimpleC.Util

-- | Concrete Memory Cell
type ConMCell = MemCell SymId () [Value]

-- | The concrete domain 
--   The concrete domain is a variation of 
--   the Powerset(state) where state is a 
--   pair (heap, threadstate).
data Sigma = 
  Sigma 
  { 
    heap :: IntMap ConMCell
  , th_states :: Map TId ThState
  , num_th  :: Int
  , is_bot  :: Bool 
  }
  deriving Show
 
-- | A thread state is a control and local data 
data ThState =
  ThState
  { 
    pos :: Pos
  , locals :: Map SymId [Value]
  } 
  deriving (Show,Eq,Ord)

-- | Checks for state subsumption
-- 1. Check bottoms 
-- 2. Check if the number of threads
--    is greater or equal
-- 3. Check the heap
-- 4. Check the thread states
subsumes_concrete :: Sigma -> Sigma -> Bool
subsumes_concrete st1 st2 =
  case check_bottoms (is_bot st1) (is_bot st2) of
    Just r -> r
    Nothing ->
      if (num_th st1) < (num_th st2)
      then False
      else
        let sts1 = th_states st1
            hp1 = heap st1
        in if M.foldrWithKey' (\tid th b -> check_threads tid th sts1 && b) True (th_states st2)
           then IM.foldrWithKey' (\mid mcell b -> check_heap mid mcell hp1 && b) True (heap st2)
           else False 
 where
   check_bottoms b1 b2 =
     if b1 
     then Just b2
     else if b2
          then Just True
          else Nothing
   check_threads tid th2 sts1 =
     case M.lookup tid sts1 of
       Nothing -> False
       Just th1 ->
         let lcs1 = locals th1
         in if pos th1 == pos th2
            then M.foldrWithKey' (\sym vals b -> check_locals sym vals lcs1 && b) True (locals th2)  
            else False
   check_locals sym val2 lcs1 =
     case M.lookup sym lcs1 of
       Nothing -> False
       Just val1 -> all (\v -> v `elem` val1) val2
   check_heap mid cell2 hp1 =
     case IM.lookup mid hp1 of
       Nothing -> False
       Just cell1 ->
         let r = ty cell1 == ty cell2
             vals1 = val cell1
         in r && all (\v -> v `elem` vals1) (val cell2)
 
instance Projection Sigma where
  controlPart st@Sigma{..} = M.map pos th_states
  subsumes a b = subsumes_concrete a b
  isBottom = is_bot 

instance Collapsible Sigma Act where
   enabled = undefined
   collapse = undefined
   dcollapse = undefined 
{-
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
-}
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
