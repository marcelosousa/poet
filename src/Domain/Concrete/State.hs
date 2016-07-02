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
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Model.GCS
import Util.Generic hiding (safeLookup)
import Language.SimpleC.AST
import Language.SimpleC.Util

-- | Scope (of a transformer)
--   It can either be global (if we processing
--   for example global declarations and so we need
--   to change the state of the heap) or it can
--   be local to a thread and we might have to
--   change the local state and the heap
data Scope = Global | Local TId

type ConValues = [ConValue]

-- | Concrete Value for the concrete semantics
data ConValue
  =  ConVal Value      -- Concrete list of values
  -- Memory address value: address
  | ConMemAddr MemAddr
  -- Array value
  -- Memory address for the positions and the size
  | ConArr [ConValue] Int Bool -- IsTop 
  deriving (Show,Eq,Ord)

-- | Concrete Memory address contains of a base + offset
-- data MemAddr
--   = MemAddr 
--   { base :: ConValue
--   , offset :: ConValue
--   }
--   deriving (Show,Eq,Ord)
-- Simplification
data MemAddr
  = MemAddr 
  { base :: SymId }
  deriving (Show,Eq,Ord)

data MemAddrs
  = MemAddrTop
  | MemAddrs [MemAddr]
  deriving (Show,Eq)

instance Ord MemAddrs where
  m1 <= m2 = case (m1,m2) of 
    (_,MemAddrTop) -> True
    (MemAddrTop,MemAddrs l) -> False 
    (MemAddrs l1,MemAddrs l2) ->
      all (\a -> a `elem` l2) l1 

bot_maddrs :: MemAddrs
bot_maddrs = MemAddrs []

is_maddrs_bot :: MemAddrs -> Bool
is_maddrs_bot maddr =
  case maddr of
    MemAddrTop -> False
    MemAddrs l -> null l
  
meet_maddrs :: MemAddrs -> MemAddrs -> MemAddrs
meet_maddrs a1 a2 =
  case (a1,a2) of
    (MemAddrTop,_) -> a2
    (_,MemAddrTop) -> a1
    (MemAddrs l1, MemAddrs l2) -> 
      MemAddrs (l1 `intersect` l2)

join_maddrs :: MemAddrs -> MemAddrs -> MemAddrs
join_maddrs a1 a2 =
  case (a1,a2) of
    (MemAddrTop,_) -> a1
    (_,MemAddrTop) -> a2
    (MemAddrs l1, MemAddrs l2) ->
      MemAddrs (nub $ l1 ++ l2)

-- | Concrete Memory Cell
type ConMCell = MemCell SymId () ConValue

-- | Concrete Heap
type ConHeap = Map SymId ConMCell

-- | The concrete domain 
--   The concrete domain is a variation of 
--   the Powerset(state) where state is a 
--   pair (heap, threadstate).
newtype CState = CState { sts :: Set Sigma }
 deriving (Show,Eq)

join_cstate :: CState -> CState -> CState
join_cstate (CState s1) (CState s2) = CState (s1 `S.union` s2) 

data Sigma = 
  Sigma 
  { 
    heap :: ConHeap 
  , th_states :: Map TId ThState
  , num_th  :: Int
  , is_bot  :: Bool 
  }
  deriving (Show,Eq,Ord)

-- | A thread state is a control and local data 
data ThState =
  ThState
  { 
    pos :: Pos
  , locals :: Map SymId ConValue 
  } 
  deriving (Show,Eq,Ord)

bot_sigma :: Sigma
bot_sigma = Sigma M.empty M.empty 0 False

bot_state :: CState
bot_state = CState S.empty

-- | Initial state which is not bottom
empty_state :: CState
empty_state = CState $ S.singleton bot_sigma 

-- | Set the position in the cfg of a thread
set_pos :: CState -> TId -> Pos -> CState
set_pos (CState st) tid npos = CState $ S.map (\s -> set_pos_s s tid npos) st

set_pos_s :: Sigma -> TId -> Pos -> Sigma
set_pos_s st@Sigma{..} tid npos = 
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
 
instance Projection Sigma where
  controlPart st@Sigma{..} = M.map pos th_states
  subsumes a b = subsumes_concrete a b
  isBottom = is_bot 

instance Projection CState where
  controlPart (CState a) =
    if S.null a
    then error "control part of bottom state"
    else let s = S.map controlPart a
         in if S.size s > 1
            then error "more than one control vector in the set"
            else S.elemAt 0 s
  subsumes (CState a) (CState b) = S.isSubsetOf b a
  isBottom (CState a) = S.null a
 
-- | API for modifying the state

-- | insert_heap: inserts an element to the heap
insert_heap :: Sigma -> SymId -> STy -> ConValues -> CState 
insert_heap st sym ty vals =
  if null vals
  then error "insert_heap: no values"
  else let sts = map (insert_heap_sigma st sym ty) vals
       in CState $ S.fromList sts

insert_heap_sigma :: Sigma -> SymId -> STy -> ConValue -> Sigma 
insert_heap_sigma st@Sigma{..} sym ty val =
  let cell = MCell ty val
      heap' = M.insert sym cell heap
  in st { heap = heap' }

modify_heap :: Sigma -> SymId -> ConValue -> Sigma 
modify_heap st@Sigma{..} id val = 
  let heap' = M.update (update_conmcell val) id heap
  in st {heap = heap'}

update_conmcell :: ConValue -> ConMCell -> Maybe ConMCell
update_conmcell nval c@MCell{..} = Just $ c { val = nval } 

-- | insert_local: inserts an element to local state 
insert_local :: Sigma -> TId -> SymId -> ConValues -> CState 
insert_local st tid sym vals =
  if null vals
  then error "insert_local: no values"
  else let sts = map (insert_local_sigma st tid sym) vals
       in CState $ S.fromList sts

insert_local_sigma :: Sigma -> TId -> SymId -> ConValue -> Sigma 
insert_local_sigma st@Sigma{..} tid sym val =
  case M.lookup tid th_states of
    Nothing -> error "insert_local_sigma: tid not found in th_states"
    Just s@ThState{..} ->
      let locals' = M.insert sym val locals
          s' = s { locals = locals' }
          th_states' = M.insert tid s' th_states
      in st { th_states = th_states' } 

-- | modify the state: receives a MemAddrs and a
--   ConValue and assigns the ConValue to the MemAddrs
modify_state :: Scope -> Sigma -> MemAddrs -> ConValues -> CState 
modify_state scope st addrs vals = 
  case addrs of
    MemAddrTop -> error "modify_state: top addrs, need to traverse everything"
    MemAddrs l -> case l of
      [] -> error "modify_state: list of addresses is empty"
      [a@MemAddr{..}] ->
        if null vals
        then error "modify_state: null vals"
        else let sts = map (modify_local_sigma scope st base) vals
             in CState $ S.fromList sts 
      _ -> error "modify_state: list of addresses contains more than one"

modify_local_sigma :: Scope -> Sigma -> SymId -> ConValue -> Sigma
modify_local_sigma scope st@Sigma{..} sym val =
  -- First search in the heap 
  case M.lookup sym heap of
    Nothing ->
      -- If not in the heap, search in the thread
      case scope of
        Global -> error "modify_state: id is not the heap and scope is global"
        Local i -> insert_local_sigma st i sym val 
    Just _ -> modify_heap st sym val

checkBoolVals :: ConValues -> (Bool,Bool)
checkBoolVals vals = (any isTrue vals, any isFalse vals)

isTrue :: ConValue -> Bool
isTrue val = case val of
  ConVal v -> case v of
    VBool b -> b
    _ -> False 
  _ -> False  

isFalse :: ConValue -> Bool
isFalse val = case val of
  ConVal v -> case v of
    VBool b -> not b
    _ -> False 
  _ -> False 
 
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
