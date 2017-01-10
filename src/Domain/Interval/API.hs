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
module Domain.Interval.API where

import Control.Monad.State.Lazy
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Domain.Interval.State
import Domain.Interval.Value
import Domain.MemAddr
import Domain.Util
import Language.C.Syntax.Ops 
import Language.SimpleC.AST
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.Map as M
import qualified Debug.Trace as T

mtrace a b = b

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

-- | Write to memory: receives a IntMAddrs and a
--   IntValue and assigns the IntValue to the MemAddrs
write_memory :: IntState -> IntMAddrs -> IntValue -> IntState
write_memory st addrs vals = mtrace ("write_memory: addrs = " ++ show addrs) $ 
  case addrs of
    MemAddrTop -> error "write_memory: top addrs, need to traverse everything"
    MemAddrs l -> foldr (\a s -> write_memory_addr s a vals) st l

-- | Write to memory of one address
write_memory_addr :: IntState -> IntMAddr -> IntValue -> IntState
write_memory_addr st@IntState{..} addr@MemAddr{..} conval =
  mtrace ("write_memory_addr: addr = " ++ show addr ++ ", val = " ++ show conval) $
  if range offset == 1
  then case level of
    Global -> modify_heap st addr conval 
    Local i -> insert_local st i addr conval 
  else
    let addrs = concretize_addr addr
    in write_memory st addrs conval 

-- @TODO: Return a list of a just a intvalue by doing a join over all results?
read_memory :: IntState -> IntMAddrs -> [IntValue]
read_memory st addrs = mtrace ("read_memory: " ++ show addrs) $  
  case addrs of 
    MemAddrTop -> error "read_memory: have not implemented MemAddrTop"
    MemAddrs l -> map (read_memory_addr_just st) l

read_memory_addr_just :: IntState -> IntMAddr -> IntValue
read_memory_addr_just st addr =
  case read_memory_addr st addr of
    Nothing -> error $ "read_memory_addr: " ++ show addr ++ " not found"  
    Just v  -> v

read_memory_addr :: IntState -> IntMAddr -> Maybe IntValue
read_memory_addr st addr = mtrace ("read_memaddr: addr = " ++ show addr ) $
  case level addr of
    Global -> case M.lookup addr (heap st) of 
      Nothing   ->  
        -- Check if we can concretize the addrs
        if range (offset addr) == 1
        then error $ "read_memaddr: " ++ show addr ++ " not found"
        else 
          let addrs = concretize_addr addr
              vals = read_memory st addrs
          in Just $ join_intval_list vals
      Just cell -> Just $ val cell
    Local tid -> case M.lookup tid (th_states st) of   
      Nothing -> error $ "read_memaddr: tid " ++ show tid ++ " not found, addr " ++ show addr
      Just th -> case M.lookup addr (th_locals th) of
        Nothing -> 
          -- Check if the variable is in the heap
          let addr' = addr { level = Global }
          in case read_memory_addr st addr' of
            Nothing -> 
              -- Check if we can concretize the addrs
              if range (offset addr) == 1
              then error $ "read_memaddr: " ++ show addr ++ " not found"
              else 
                let addrs = concretize_addr addr
                    vals = read_memory st addrs
                in Just $ join_intval_list vals
            Just vals -> Just vals
        Just value -> Just $ value 

concretize_addr :: IntMAddr -> IntMAddrs
concretize_addr a@MemAddr{..} =
  let offsets = concretize_interval offset
  in MemAddrs $ map (set_offset a) offsets

-- | Checks if an address is initialized in some part of the memory
is_present :: Show a => Map IntMAddr a -> IntMAddr -> Bool
is_present m k = case M.lookup k m of
   Nothing -> False
   Just i  -> True

-- | API FOR HEAP
-- | insert_heap: inserts an element to the heap
insert_heap :: IntState -> IntMAddr -> STy -> IntValue -> IntState
insert_heap st@IntState{..} addr ty val =
  let cell = MCell ty val 
      heap' = M.insert addr cell heap
  in st {heap = heap'}  

modify_heap :: IntState -> IntMAddr -> IntValue -> IntState
modify_heap st@IntState{..} addr val = 
  let heap' = M.update (update_conmcell val) addr heap
  in st {heap = heap'}
 where
   update_conmcell :: IntValue -> IntMCell -> Maybe IntMCell
   update_conmcell nval c@MCell{..} = Just $ c { val = nval } 

-- | API FOR THREAD STATE
-- | insert_local: inserts an element to local state 
insert_local :: IntState -> TId -> IntMAddr -> IntValue -> IntState
insert_local st@IntState{..} tid addr val = 
 case M.lookup tid th_states of
    Nothing -> error "insert_local: tid not found in th_states"
    Just s@ThState{..} ->
      let locals' = M.insert addr val th_locals
          s' = s { th_locals = locals' }
          th_states' = M.insert tid s' th_states
      in st { th_states = th_states' }

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
