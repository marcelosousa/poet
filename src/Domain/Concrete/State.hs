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
  deriving (Show,Eq,Ord)

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
-- @Add Scope to MemAddr!
data MemAddr
  = MemAddr 
  { base :: SymId
  , level :: Scope }
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
  , th_states :: ThStates
  , num_th  :: Int
  , is_bot  :: Bool 
  }
  deriving (Show,Eq,Ord)

-- | A thread state is a control and local data
type ThStates = Map TId ThState
type Locals = Map SymId ConValue 
data ThState =
  ThState
  { 
    pos :: Pos
  , id :: SymId
  , locals :: Locals 
  } 
  deriving (Show,Eq,Ord)

bot_th_state :: Pos -> SymId -> ThState
bot_th_state pos id = ThState pos id M.empty

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

inc_num_th :: Sigma -> (Int,Sigma)
inc_num_th s@Sigma{..} =
  let n = num_th + 1
  in (n,s { num_th = n })

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

insert_thread :: Sigma -> SymId -> Pos -> CState
insert_thread s sym pos =
  let (tid,s'@Sigma{..}) = inc_num_th s
      th = bot_th_state pos sym 
      th_states' = M.insert tid th th_states
      ns = s' { th_states = th_states' }
  in CState $ S.singleton ns 
   
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

-- Arithmetic operations in ConValue
add_conval, sub_conval, mult_conval :: ConValue -> ConValue -> ConValue
add_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ add_value v1 v2
  _  -> error "add_conval: not ConVal"
sub_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ sub_value v1 v2
  _  -> error "sub_ConVal: not ConVal"
mult_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ mult_value v1 v2
  _  -> error "mult_ConVal: not ConVal"
divs_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ div_value v1 v2
  _  -> error "div_ConVal: not ConVal"
rmdr_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ rmd_value v1 v2
  _  -> error "rmdr_ConVal: not ConVal"
minus_conval c1 = case c1 of
  ConVal v1 -> ConVal $ minus_value v1
  _ -> error "minus_conval: not ConVal"

-- Boolean operations in ConValue
neg_conval c1 = case c1 of
  ConVal v -> ConVal $ neg_value v 

le_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ le_value v1 v2
  _ -> error "le_conval: not conVal" 
gr_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ gr_value v1 v2
  _ -> error "gr_conval: not conVal"
leq_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ lor_value (gr_value v1 v2) (eq_value v1 v2) 
  _ -> error "leq_conval: not conVal"
geq_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ lor_value (gr_value v1 v2) (eq_value v1 v2) 
  _ -> error "geq_conval: not conVal"
eq_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ eq_value v1 v2 
  _ -> error "eq_conval: not conVal"
neq_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ neg_value $ eq_value v1 v2 
  _ -> error "neq_conval: not conVal"
land_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ land_value v1 v2 
  _ -> error "neq_conval: not conVal"
lor_conval c1 c2 = case (c1,c2) of
  (ConVal v1,ConVal v2) -> ConVal $ lor_value v1 v2 
  _ -> error "neq_conval: not conVal"

instance Hashable CState where
  hash (CState sts) = hash $ S.toList sts 
  hashWithSalt s (CState sts) = hashWithSalt s $ S.toList sts

instance Hashable Sigma where
  hash s@Sigma{..} = hash (heap,th_states,num_th,is_bot) 
  hashWithSalt s st@Sigma{..} = hashWithSalt s (heap,th_states,num_th,is_bot)

instance Hashable ConHeap where
  hash = hash . M.toList 
  hashWithSalt s h = hashWithSalt s $ M.toList h

instance Hashable ThStates where
  hash = hash . M.toList 
  hashWithSalt s th = hashWithSalt s $ M.toList th

instance Hashable ThState where
  hash th@ThState{..} = hash (pos,id,locals)
  hashWithSalt s th@ThState{..} = hashWithSalt s (pos,id,locals)

instance Hashable Locals where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h
 
instance Hashable SymId where
  hash (SymId i) = hash i
  hashWithSalt s (SymId i) = hashWithSalt s i

instance Hashable ConMCell where
  hash m@MCell{..} = hash val
  hashWithSalt s m@MCell{..} = hashWithSalt s val

instance Hashable ConValue where
  hash v = case v of
    ConVal val -> hash val
    ConMemAddr mem -> hash mem
    _ -> error "hash not supported" 
  hashWithSalt s v = case v of
    ConVal val -> hashWithSalt s val
    ConMemAddr mem -> hashWithSalt s mem
    _ -> error "hash not supported" 
 
instance Hashable Value where
  hash v = case v of
    VInt    i -> hash i 
    VShort  i -> hash i 
    VLong   i -> hash i 
    VDouble i -> hash i 
    VFloat  i -> hash i 
    VBool   i -> hash i 
    VChar   i -> hash i 
    VString i -> hash i 
  hashWithSalt s v = case v of
    VInt    i -> hashWithSalt s i 
    VShort  i -> hashWithSalt s i 
    VLong   i -> hashWithSalt s i 
    VDouble i -> hashWithSalt s i 
    VFloat  i -> hashWithSalt s i 
    VBool   i -> hashWithSalt s i 
    VChar   i -> hashWithSalt s i 
    VString i -> hashWithSalt s i 

instance Hashable MemAddr where
  hash m@MemAddr{..} = hash base
  hashWithSalt s m@MemAddr{..} = hashWithSalt s base

{- 
-- State equality: because I'm using a hashtable I need to stay within the ST monad
isEqual :: Sigma s -> Sigma s -> ST s Bool
isEqual s1 s2 = do
  l1 <- H.toList s1
  l2 <- H.toList s2
  return $ isEqual' l1 l2  
-}
