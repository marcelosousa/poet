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
import Domain.Interval.Offset
import Domain.Lattice
import Domain.MemAddr
import Domain.Util
import Language.C.Syntax.Ops 
import Language.SimpleC.AST
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Debug.Trace as T

-- | Interval Memory Cell
type IntMCell = MemCell SymId () IntValue

instance Show IntMCell where
  show (MCell _ val) = show val

-- | Interval Heap
-- IntOffs represents a mapping from memory offsets to interval values
type IntOffs = Map IntValue IntValue 
 
gen_offset_lists :: IntMemoryFn -> IntOffs -> IntOffs -> IntOffs
gen_offset_lists fn m1 m2 =
  M.fromList $ fn (M.toList m1) (M.toList m2) 
  
instance Lattice IntOffs where
   bot   = M.empty
   top   = error "top for IntOffs"
   join  = gen_offset_lists join
   meet  = gen_offset_lists meet
   widen = gen_offset_lists widen
   
type IntMemoryFn = IntOffsList -> IntOffsList -> IntOffsList

-- Old type
-- type IntHeap = Map IntMAddr IntMCell
-- An interval heap is a map from base addresses to a map of offsets to values
type IntMemory = Map MemAddrBase IntOffs 

instance Lattice IntMemory where
   bot   = M.empty
   top   = error "top for IntMemory"
   join  = M.unionWith join 
   meet  = M.intersectionWith meet
   widen = M.unionWith widen
   
-- | A thread state is a control and local data
type ThStates = Map TId ThState
data ThState =
  ThState
  { 
    th_pos    :: Pos
  , th_cfg_id :: SymId 
  , th_locals :: IntMemory 
  } 
  deriving (Show,Eq,Ord)

bot_th_state :: Pos -> SymId -> ThState
bot_th_state pos cfg_id = ThState pos cfg_id bot

ite :: Eq a => String -> a -> a -> a
ite e_str a b
  | a == b = a
  | otherwise = error e_str
  
instance Lattice ThState where
   bot   = error "bot for ThState"
   top   = error "top for ThState"
   join  t1 t2 =
     let _pos    = ite ("join_intthst:\n" ++ show t1 ++ "\n" ++ show t2)    (th_pos t1) (th_pos t2) 
         _cfg_id = ite "join_intthst:" (th_cfg_id t1) (th_cfg_id t2) 
         _locals = (th_locals t1) `join` (th_locals t2) 
     in ThState _pos _cfg_id _locals
   meet  = error "meet for ThState"
   widen t1 t2 =
     let _pos    = ite "widen_intthst:" (th_pos    t1) (th_pos    t2) 
         _cfg_id = ite "widen_intthst:" (th_cfg_id t1) (th_cfg_id t2) 
         _locals = M.unionWith widen (th_locals t1) (th_locals t2) 
     in ThState _pos _cfg_id _locals

instance Lattice ThStates where
   bot   = M.empty
   top   = error "top for ThStates"
   join  = M.unionWith        join
   meet  = M.intersectionWith meet
   widen = M.unionWith        widen

-- | The interval domain 
--   The interval domain is a 
--   pair (heap, threadstate).
data IntState = 
  IntState 
  { 
    heap      :: IntMemory 
  , th_states :: ThStates
  , num_th    :: Int
  , is_bot    :: Bool 
  }
  deriving Eq

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
-- Returns true if st1 >= st2
instance Ord IntState where
   (<=) a b = b `subsumes_interval` a
   
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
        in if M.foldrWithKey' (\tid th b -> 
                check_threads tid th sts1 && b) True (th_states st2)
           then M.foldrWithKey' (\mid offs b -> 
                  check_offsets mid offs hp1 && b) True (heap st2)
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
          then M.foldrWithKey' (\sym offs b -> 
                 check_offsets sym offs lcs1 && b) True (th_locals th2) 
          else False
   check_offsets mid offs2 hp1 =
     case M.lookup mid hp1 of
       Nothing -> False
       Just offs1 ->
         let moffs2 = M.toList offs2
             moffs1 = M.toList offs1
         in subsumes_intval_list moffs1 moffs2 

instance Lattice IntState where
   bot         = IntState bot bot 0 True
   top         = error "top for IntState"
   join  s1 s2 = 
     case ((?.) s1, (?.) s2) of
       (True,_) -> s2
       (_,True) -> s1
       -- They are not bot
       _ -> let _heap      = (heap s1)    `join` (heap s2)
                _th_states = th_states s1 `join` th_states s2
                _num_th    = M.size _th_states 
                _is_bot    = False
            in IntState _heap _th_states _num_th _is_bot
   meet  s1 s2 = error "meet for IntState"
   widen s1 s2 = 
     if s1 == s2 
     then s1
     else let _heap      = (heap s1)    `widen` (heap s2)
              _th_states = th_states s1 `widen` th_states s2
              _num_th    = M.size _th_states 
              _is_bot    = False
          in IntState _heap _th_states _num_th _is_bot
   (?.) = is_bot
   
-- | Projection instance
instance Projection IntState where
  controlPart st@IntState{..} = M.map th_pos th_states
  toThCFGSym st@IntState{..} tid = 
    case M.lookup tid th_states of
      Nothing -> error $ "toThCFGSym: invalid tid " ++ show tid
      Just t@ThState{..} -> th_cfg_id 
 
-- | Pretty Printing
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

-- | Hashable instances 
instance Hashable IntState where
  hash s@IntState{..} = hash (heap,th_states,num_th,is_bot) 
  hashWithSalt s st@IntState{..} = hashWithSalt s (heap,th_states,num_th,is_bot) 

instance Hashable IntMemory where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h
 
instance Hashable IntOffs where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h

instance Hashable ThStates where
  hash = hash . M.toList
  hashWithSalt s th = hashWithSalt s $ M.toList th

instance Hashable ThState where
  hash th@ThState{..} = hash (th_pos,th_cfg_id,th_locals)
  hashWithSalt s th@ThState{..} = hashWithSalt s (th_pos,th_cfg_id,th_locals)

instance Hashable IntMCell where
  hash m@MCell{..} = hash val
  hashWithSalt s m@MCell{..} = hashWithSalt s val

