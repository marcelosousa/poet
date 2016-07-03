{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.State
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- The domain for the interval semantics.
-------------------------------------------------------------------------------
module Domain.Interval.State where

import Data.Hashable
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Model.GCS
import Util.Generic hiding (safeLookup)
import Language.SimpleC.AST
import Language.SimpleC.Util

import Domain.Util

-- | Interval Memory Cell
type IntMCell = MemCell SymId () IntValue

-- | Interval Heap
type IntHeap = Map SymId IntMCell

-- | The interval domain 
--   The interval domain is a 
--   pair (heap, threadstate).
data IntState = 
  IntState 
  { 
    heap :: IntHeap 
  , th_states :: ThStates
  , num_th  :: Int
  , is_bot  :: Bool 
  }
  deriving Show

-- | A thread state is a control and local data
type ThStates = Map TId ThState
type Locals = Map SymId IntValue 
data ThState =
  ThState
  { 
    pos :: Pos
  , id :: Tid
  , locals :: Locals 
  } 
  deriving (Show,Eq,Ord)

-- | Initial state which is not bottom
empty_state :: IntState 
empty_state = IntState M.empty M.empty 0 False 

-- | Set the position in the cfg of a thread
set_pos :: IntState -> TId -> Pos -> Sigma
set_pos st@IntState{..} tid npos = 
  let th_st' =
        case M.lookup tid th_states of
          Nothing -> error "set_pos: tid not in th_states"
          Just t@ThState{..} ->
            let pos' = npos
            in t { pos = pos' }
      th_states' = M.insert tid th_st' th_states 
  in st { th_states = th_states' }

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
 
instance Projection IntState where
  controlPart st@IntState{..} = M.map pos th_states
  subsumes a b = subsumes_interval a b
  isBottom = is_bot 

-- | API for modifying the state

{-
-- | insert_heap: inserts an element to the heap
insert_heap :: Sigma -> SymId -> ConMCell -> Sigma
insert_heap st@Sigma{..} id cell =
  let heap' = M.insert id cell heap
  in st {heap = heap'}  

modify_heap :: Sigma -> SymId -> ConValue -> Sigma
modify_heap st@Sigma{..} id val = 
  let heap' = M.update (update_conmcell val) id heap
  in st {heap = heap'}

update_conmcell :: ConValue -> ConMCell -> Maybe ConMCell
update_conmcell nval c@MCell{..} = Just $ c { val = nval } 

-- | insert_local: inserts an element to local state 
insert_local :: Sigma -> TId -> SymId -> ConValue -> Sigma
insert_local = error "insert_local" 

-- | modify the state: receives a MemAddrs and a
--   ConValue and assigns the ConValue to the MemAddrs
modify_state :: Scope -> Sigma -> MemAddrs -> ConValue -> Sigma
modify_state scope st addrs vals =
  case addrs of
    MemAddrTop -> error "modify_state: top addrs, need to traverse everything"
    MemAddrs l -> foldr (\a s -> modify_state' scope s a vals) st l
 where
   modify_state' :: Scope -> Sigma -> MemAddr -> ConValue -> Sigma
   modify_state' scope st@Sigma{..} add@MemAddr{..} conval =
     -- First search in the heap 
     case M.lookup base heap of
       Nothing ->
         -- If not in the heap, search in the thread
         case scope of
           Global -> error "modify_state: id is not the heap and scope is global"
           Local i -> insert_local st i base conval 
       Just _ -> modify_heap st base conval  
-} 
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
