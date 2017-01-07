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
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Domain.Interval.Value
import Domain.Util
import Language.C.Syntax.Ops 
import Language.SimpleC.AST
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.IntMap as IM
import qualified Data.Map as M

-- | Interval Memory Cell
type IntMCell = MemCell SymId () IntValue

instance Show IntMCell where
  show (MCell _ val) = show val

-- | Interval Heap
type IntHeap = Map IntMAddr IntMCell

-- | A thread state is a control and local data
type ThStates = Map TId ThState
type Locals = Map IntMAddr IntValue 
data ThState =
  ThState
  { 
    th_pos    :: Pos
  , th_cfg_id :: SymId 
  , th_locals :: Locals 
  } 
  deriving (Show,Eq,Ord)

bot_th_state :: Pos -> SymId -> ThState
bot_th_state pos cfg_id = ThState pos cfg_id M.empty

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
  deriving Eq

set_int_state_bot :: IntState -> IntState
set_int_state_bot i = i { is_bot = True }

h_u = "##############################################\n"
h_d = "----------------------------------------------\n"
instance Show IntState where
  show (IntState h s nt b) =
    let h_s = if M.null h 
              then "\t\tEMPTY HEAP\n" ++ h_d
              else "\t\tHEAP\n" ++ h_d ++ showMem h ++ h_d
        s_s = "\t\tTHREADS(" ++ show nt ++ ")\n" ++ h_d ++ showThStates s
        i_s = h_u ++ "\t\tSTATE IS BOTTOM = " ++ show b ++ "\n"++ h_d 
    in i_s ++ h_s ++ "\n" ++ s_s ++ h_u 

showMem s = M.foldWithKey (\k m r -> "    " ++ show k ++ " := " ++ show m ++ "\n" ++ r) "" s

showThStates s = 
  M.foldWithKey (\k t r -> "   Thread: PID = " ++ show k ++ showThState t ++ r) "" s
 where 
   showThState (ThState p c s) = 
     let p_s = ", LOC = " ++ show p
         c_s = ", CFG ID = " ++ show c
         l_s = "\n" ++ showMem s
     in p_s ++ c_s ++ l_s ++ "\n" ++ h_d 

-- | Domain operations
-- | Initial state which is not bottom
empty_state :: IntState 
empty_state = IntState M.empty M.empty 1 False 

-- | Checks for state subsumption
-- 1. Check bottoms 
-- 2. Check if the number of threads
--    is greater or equal
-- 3. Check the heap
-- 4. Check the thread states
subsumes_interval :: IntState -> IntState -> Bool
subsumes_interval st1 st2 =
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
       let lcs1 = th_locals th1
       in if th_pos th1 == th_pos th2
          then M.foldrWithKey' (\sym v b -> check_locals sym v lcs1 && b) True (th_locals th2) 
          else False
   check_locals :: IntMAddr -> IntValue -> Locals -> Bool 
   check_locals sym val2 lcs1 =
     case M.lookup sym lcs1 of
       Nothing -> False
       Just val1 -> val2 <= val1 
   check_heap mid cell2 hp1 =
     case M.lookup mid hp1 of
       Nothing -> False
       Just cell1 ->
         let r = ty cell1 == ty cell2
             vcell al1 = val cell1
             val1 = val cell1
             val2 = val cell2
         in r && val2 <= val1
 
-- | Join operation
join_intstate :: IntState -> IntState -> IntState
join_intstate s1 s2 = case (is_bot s1, is_bot s2) of
  (True,_) -> s2
  (_,True) -> s1
  -- They are not bot
  _ -> let _heap = (heap s1) `join_intheap` (heap s2)
           _th_states = th_states s1 `join_intthsts` th_states s2
           -- This is not correct in general
           _num_th = max (num_th s1) (num_th s2) 
           _is_bot = False
       in IntState _heap _th_states _num_th _is_bot

join_intheap :: IntHeap -> IntHeap -> IntHeap
join_intheap m1 m2 = M.unionWith join_intmcell m1 m2 

join_intthsts :: ThStates -> ThStates -> ThStates
join_intthsts = M.unionWith join_intthst 

ite :: Eq a => String -> a -> a -> a
ite e_str a b
  | a == b = a
  | otherwise = error e_str

join_intthst :: ThState -> ThState -> ThState
join_intthst t1 t2 =
  let _pos    = ite "join_intthst: diff th_pos"    (th_pos    t1) (th_pos    t2) 
  --    _id     = ite "join_intthst: diff th_id"     (th_id     t1) (th_id     t2) 
      _cfg_id = ite "join_intthst: diff th_cfg_id" (th_cfg_id t1) (th_cfg_id t2) 
      _locals = M.unionWith iJoin (th_locals t1) (th_locals t2) 
  in ThState _pos _cfg_id _locals

join_intmcell :: IntMCell -> IntMCell -> IntMCell
join_intmcell m1 m2 =
  let _ty = ty m1
      _val = (val m1) `iJoin` (val m2)
  in MCell _ty _val

-- | Projection instance
instance Projection IntState where
  controlPart st@IntState{..} = M.map th_pos th_states
  subsumes a b = subsumes_interval a b
  isBottom = is_bot 
  toThCFGSym st@IntState{..} tid = 
    case M.lookup tid th_states of
      Nothing -> error $ "toThCFGSym: invalid tid " ++ show tid
      Just t@ThState{..} -> th_cfg_id 
 
-- | Hashable instances 
instance Hashable IntState where
  hash s@IntState{..} = hash (heap,th_states,num_th,is_bot) 
  hashWithSalt s st@IntState{..} = hashWithSalt s (heap,th_states,num_th,is_bot) 

instance Hashable IntHeap where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h
 
instance Hashable ThStates where
  hash = hash . M.toList
  hashWithSalt s th = hashWithSalt s $ M.toList th

instance Hashable ThState where
  hash th@ThState{..} = hash (th_pos,th_cfg_id,th_locals)
  hashWithSalt s th@ThState{..} = hashWithSalt s (th_pos,th_cfg_id,th_locals)

instance Hashable Locals where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h

instance Hashable IntMCell where
  hash m@MCell{..} = hash val
  hashWithSalt s m@MCell{..} = hashWithSalt s val

