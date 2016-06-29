{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.State
-- Copyright :  (c) 2015-16 Marcelo Sousa
--
-- The domain for the concrete semantics.
-------------------------------------------------------------------------------
module Domain.Concrete.State where

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

-- | Concrete Value for the concrete semantics
data ConValue
  = ConTop              -- Top value 
  | ConBot              -- Bot value
  | ConVal [Value]      -- Concrete list of values
  -- Memory address value
  | ConMemAddr ConValue         -- Base 
               (Maybe ConValue) -- Maybe Offset
  -- Array value
  -- Memory address for the positions and the size
  | ConArr [ConValue] Int Bool -- IsTop 
  deriving (Show,Eq,Ord)

-- | Concrete Memory Cell
type ConMCell = MemCell SymId () ConValue

-- | Concrete Heap
type ConHeap = Map SymId ConMCell

-- | The concrete domain 
--   The concrete domain is a variation of 
--   the Powerset(state) where state is a 
--   pair (heap, threadstate).
data Sigma = 
  Sigma 
  { 
    heap :: ConHeap 
  , th_states :: Map TId ThState
  , num_th  :: Int
  , is_bot  :: Bool 
  }
  deriving Show

-- | Initial state which is not bottom
empty_state :: Sigma
empty_state = Sigma M.empty M.empty 0 False 

-- | A thread state is a control and local data 
data ThState =
  ThState
  { 
    pos :: Pos
  , locals :: Map SymId ConValue 
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
           then M.foldrWithKey' (\mid mcell b -> check_heap mid mcell hp1 && b) True (heap st2)
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
   check_locals :: SymId -> ConValue -> Map SymId ConValue -> Bool 
   check_locals sym val2 lcs1 =
     case M.lookup sym lcs1 of
       Nothing -> False
       Just val1 -> val2 <= val1 
   check_heap mid cell2 hp1 =
     case M.lookup mid hp1 of
       Nothing -> False
       Just cell1 ->
         let r = ty cell1 == ty cell2
             val1 = val cell1
             val2 = val cell2
         in r && val2 <= val1
 
instance Projection Sigma where
  controlPart st@Sigma{..} = M.map pos th_states
  subsumes a b = subsumes_concrete a b
  isBottom = is_bot 

-- | API for modifying the state

-- | insert_heap: inserts an element to the heap
insert_heap :: Sigma -> SymId -> ConMCell -> Sigma
insert_heap st@Sigma{..} id cell =
  let heap' = M.insert id cell heap
  in st {heap = heap'}  

{-
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

-- State equality: because I'm using a hashtable I need to stay within the ST monad
isEqual :: Sigma s -> Sigma s -> ST s Bool
isEqual s1 s2 = do
  l1 <- H.toList s1
  l2 <- H.toList s2
  return $ isEqual' l1 l2  
-}
