{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.API
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- This module defines the API functions
-- to manipulate concrete values and states 
-------------------------------------------------------------------------------
module Domain.Concrete.API (inc_num_th, insert_thread, get_addrs, read_memory, write_memory, write_memory_addr, set_pos, update_pc) where

import Control.Monad.State.Lazy
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Domain.Concrete.State
import Domain.Concrete.Value
import Domain.MemAddr
import Domain.Util
import Language.SimpleC.AST (SymId)
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.Map as M

-- | API FOR CONCRETE STATE
-- | Increment the thread counter
inc_num_th :: ConState -> (Int, ConState)
inc_num_th s@ConState{..} =
  let n = cs_numth + 1
  in (n,s { cs_numth = n })

-- | Insert a thread into the state
insert_thread :: ConState -> TId -> ThState -> ConState 
insert_thread s@ConState{..} tid th =
  let th_states = M.insert tid th cs_tstates
  in s { cs_tstates = th_states }

-- | API TO GET FULL ADDRESSES FROM BASE ADDRESSES
-- | Converts a map of sequences and a base address into the full set of addresses
to_mem_addrs :: ConOffs -> MemAddrBase -> ConMAddrs 
to_mem_addrs offs base = MemAddrs $ M.foldrWithKey (\off _ -> (to_addr base off:)) [] offs 

-- | Gets the full memory addresses from a memory region
get_addrs_memory :: ConHeap -> MemAddrBase -> Maybe ConMAddrs
get_addrs_memory mem base = do
  offs <- M.lookup base mem
  return $ to_mem_addrs offs base

-- | Get the set of (full) memory addresses given an address base from the entire state
get_addrs :: ConState -> MemAddrBase -> ConMAddrs
get_addrs st addr = mytrace False ("get_addrs: " ++ show addr) $ 
  case _level addr of
    Local i -> 
      case M.lookup i (cs_tstates st) of
        Nothing -> error $ "get_addrs: tid = " ++ show i ++ " not found"
        Just th -> case get_addrs_memory (th_locals th) addr of
           Nothing -> get_addrs st (set_level addr Global) 
           Just as -> as 
    Global  -> case get_addrs_memory (cs_heap st) addr of
      Nothing -> error $ "get_addrs: " ++ show addr
      Just as -> as 

-- | API TO READ THE CONTENTS OF THE MEMORY
read_memory :: ConState -> ConMAddrs -> [ConValue]
read_memory st addrs = mytrace False ("read_memory: " ++ show addrs) $  
  case addrs of 
    MemAddrTop -> error "read_memory: have not implemented MemAddrTop"
    MemAddrs l -> map (read_memory_addr st) l

-- LOW LEVEL API TO READ THE CONTENTS OF THE MEMORY 
read_memory_addr :: ConState -> ConMAddr -> ConValue
read_memory_addr st addr = mytrace False ("read_memaddr: addr = " ++ show addr ) $
  let (base_addr, off) = from_addr addr 
  in case _level base_addr of
    Global -> case read_addr_from_region (cs_heap st) base_addr off of 
      Nothing  -> error $ "read_memory_addr: " ++ show addr ++ " not found"
      Just val -> val 
    Local tid -> case M.lookup tid (cs_tstates st) of   
      Nothing -> error $ "read_memory_addr: tid " ++ show tid ++ " not found, addr " ++ show addr
      Just th -> case read_addr_from_region (th_locals th) base_addr off of
        Nothing -> 
          -- Check if the variable is in the heap
          let addr' = addr { level = Global }
          in read_memory_addr st addr' 
        Just val -> val

read_addr_from_region :: ConHeap -> MemAddrBase -> ConValue -> Maybe ConValue
read_addr_from_region mem addr off = do
  offs <- M.lookup addr mem
  case M.lookup off offs of
    Just val -> Just val 
    Nothing  -> error $ "read_addr_from_region: "

-- | API TO WRITE TO MEMORY
-- | Write to memory: receives a IntMAddrs and a
--   IntValue and assigns the IntValue to the MemAddrs
write_memory :: ConState -> ConMAddrs -> ConValue -> ConState
write_memory st addrs vals = mytrace False ("write_memory: addrs = " ++ show addrs) $ 
  case addrs of
    MemAddrTop -> error "write_memory: top addrs, need to traverse everything"
    MemAddrs l -> foldr (\a s -> write_memory_addr s a vals) st l

-- | Write to memory of one address
write_memory_addr :: ConState -> ConMAddr -> ConValue -> ConState
write_memory_addr st addr val = 
  mytrace False ("write_memory_addr: addr = " ++ show addr ++ ", val = " ++ show val) $
  let (base_addr, off) = from_addr addr 
  in case _level base_addr of
    Global -> let _heap = write_region_addr (cs_heap st) base_addr off val 
              in st { cs_heap = _heap }
    Local i -> case M.lookup i (cs_tstates st) of
       Nothing -> error $ "write_memory_addr: tid = " ++ show i ++ " not found in th_states"
       Just th -> let _locals = write_region_addr (th_locals th) base_addr off val 
                      _th = th { th_locals = _locals }
                      _th_states = M.insert i _th (cs_tstates st)
                  in st { cs_tstates = _th_states }

write_region_addr :: ConHeap -> MemAddrBase -> ConValue -> ConValue -> ConHeap 
write_region_addr mem addr off val =
  let n_offs = [(off, val)]
  in case M.lookup addr mem of
    Nothing   -> M.insert addr (M.fromList n_offs) mem 
    Just offs -> let offs' = M.insert off val offs
                 in M.insert addr offs' mem  

-- | AUXILIARY FUNCTIONS
-- | Set the position in the cfg of a thread
update_pc :: ConState -> TId -> Pos -> ConState
update_pc i@ConState{..} tid pos = 
  let th_st = M.update (\(ThState _ c l) -> Just $ ThState pos c l) tid cs_tstates
  in i { cs_tstates = th_st }

-- | Set the position in the cfg of a thread
--   This can also initialize the state of a thread
set_pos :: ConState -> TId -> SymId -> Pos -> ConState 
set_pos st@ConState{..} tid cfg_sym npos = 
  let th_st' =
        case M.lookup tid cs_tstates of
          Nothing -> ThState npos cfg_sym M.empty 
          Just t@ThState{..} -> t { th_pos = npos }
      th_states' = M.insert tid th_st' cs_tstates 
  in st { cs_tstates = th_states' }
