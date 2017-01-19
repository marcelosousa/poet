{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.API
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- This module defines the API functions
-- to manipulate interval values and states 
-------------------------------------------------------------------------------
module Domain.Interval.API (inc_num_th, insert_thread, get_addrs, read_memory, write_memory, write_memory_addr, set_pos, update_pc) where

import Control.Monad.State.Lazy
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Domain.Interval.State
import Domain.Interval.Value
import Domain.MemAddr
import Domain.Util
import Language.SimpleC.AST (SymId)
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.Map as M

-- | API FOR INTERVAL STATE
-- | Increment the thread counter
inc_num_th :: IntState -> (Int, IntState)
inc_num_th s@IntState{..} =
  let n = num_th + 1
  in (n,s { num_th = n })

-- | Insert a thread into the state
insert_thread :: IntState -> TId -> ThState -> IntState 
insert_thread s@IntState{..} tid th =
  let th_states' = M.insert tid th th_states
  in s { th_states = th_states' }

-- | API TO GET FULL ADDRESSES FROM BASE ADDRESSES
-- | Converts a map of sequences and a base address into the full set of addresses
to_mem_addrs :: IntOffs -> MemAddrBase -> IntMAddrs 
to_mem_addrs offs base = MemAddrs $ M.foldrWithKey (\off _ -> (to_addr base off:)) [] offs 

-- | Gets the full memory addresses from a memory region
get_addrs_memory :: IntMemory -> MemAddrBase -> Maybe IntMAddrs
get_addrs_memory mem base = do
  offs <- M.lookup base mem
  return $ to_mem_addrs offs base

-- | Get the set of (full) memory addresses given an address base from the entire state
get_addrs :: IntState -> MemAddrBase -> IntMAddrs
get_addrs st addr = mytrace False ("get_addrs: " ++ show addr) $ 
  case _level addr of
    Local i -> 
      case M.lookup i (th_states st) of
        Nothing -> error $ "get_addrs: tid = " ++ show i ++ " not found"
        Just th -> case get_addrs_memory (th_locals th) addr of
           Nothing -> get_addrs st (set_level addr Global) 
           Just as -> as 
    Global  -> case get_addrs_memory (heap st) addr of
      Nothing -> error $ "get_addrs: " ++ show addr
      Just as -> as 

-- | API TO READ THE CONTENTS OF THE MEMORY
read_memory :: IntState -> IntMAddrs -> [IntValue]
read_memory st addrs = mytrace False ("read_memory: " ++ show addrs) $  
  case addrs of 
    MemAddrTop -> error "read_memory: have not implemented MemAddrTop"
    MemAddrs l -> map (read_memory_addr st) l

-- LOW LEVEL API TO READ THE CONTENTS OF THE MEMORY 
read_memory_addr :: IntState -> IntMAddr -> IntValue
read_memory_addr st addr = mytrace False ("read_memaddr: addr = " ++ show addr ) $
  let (base_addr, off) = from_addr addr 
  in case _level base_addr of
    Global -> case read_addr_from_region (heap st) base_addr off of 
      Nothing  -> error $ "read_memory_addr: " ++ show addr ++ " not found"
      Just val -> val 
    Local tid -> case M.lookup tid (th_states st) of   
      Nothing -> error $ "read_memory_addr: tid " ++ show tid ++ " not found, addr " ++ show addr
      Just th -> case read_addr_from_region (th_locals th) base_addr off of
        Nothing -> 
          -- Check if the variable is in the heap
          let addr' = addr { level = Global }
          in read_memory_addr st addr' 
        Just val -> val

read_addr_from_region :: IntMemory -> MemAddrBase -> IntValue -> Maybe IntValue
read_addr_from_region mem addr off = do
  offs <- M.lookup addr mem
  case M.lookup off offs of
    Just val -> Just val 
    Nothing  ->
      case M.elems $ M.filterWithKey (\k _ -> iMeet k off /= IntBot) offs of
        [] -> Nothing
        v  -> Just $ join_intval_list v 

-- | API TO WRITE TO MEMORY
-- | Write to memory: receives a IntMAddrs and a
--   IntValue and assigns the IntValue to the MemAddrs
write_memory :: IntState -> IntMAddrs -> IntValue -> IntState
write_memory st addrs vals = mytrace True ("write_memory: addrs = " ++ show addrs) $ 
  case addrs of
    MemAddrTop -> error "write_memory: top addrs, need to traverse everything"
    MemAddrs l -> foldr (\a s -> write_memory_addr s a vals) st l

-- | Write to memory of one address
write_memory_addr :: IntState -> IntMAddr -> IntValue -> IntState
write_memory_addr st addr val = 
  mytrace True ("write_memory_addr: addr = " ++ show addr ++ ", val = " ++ show val) $
  let (base_addr, off) = from_addr addr 
  in case _level base_addr of
    Global -> let _heap = write_region_addr (heap st) base_addr off val 
              in st { heap = _heap }
    Local i -> case M.lookup i (th_states st) of
       Nothing -> error $ "write_memory_addr: tid = " ++ show i ++ " not found in th_states"
       Just th -> let _locals = write_region_addr (th_locals th) base_addr off val 
                      _th = th { th_locals = _locals }
                      _th_states = M.insert i _th (th_states st)
                  in st { th_states = _th_states }

write_region_addr :: IntMemory -> MemAddrBase -> IntValue -> IntValue -> IntMemory 
write_region_addr mem addr off val =
  let n_offs = [(off, val)]
  in case M.lookup addr mem of
    Nothing   -> M.insert addr (M.fromList n_offs) mem 
    Just offs -> let res = gen_intval_list const n_offs (M.toList offs) 
                 in mytrace False ("write_region_addr: result " ++ show res) $ 
      M.insert addr (M.fromList res) mem  

-- | AUXILIARY FUNCTIONS
-- | Set the position in the cfg of a thread
update_pc :: IntState -> TId -> Pos -> IntState
update_pc i@IntState{..} tid pos = 
  let th_st = M.update (\(ThState _ c l) -> Just $ ThState pos c l) tid th_states
  in i { th_states = th_st }

-- | Set the position in the cfg of a thread
--   This can also initialize the state of a thread
set_pos :: IntState -> TId -> SymId -> Pos -> IntState 
set_pos st@IntState{..} tid cfg_sym npos = 
  let th_st' =
        case M.lookup tid th_states of
          Nothing -> ThState npos cfg_sym M.empty 
          Just t@ThState{..} -> t { th_pos = npos }
      th_states' = M.insert tid th_st' th_states 
  in st { th_states = th_states' }
