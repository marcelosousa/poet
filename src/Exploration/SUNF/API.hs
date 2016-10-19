{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Exploration.SUNF.API
-- Desc      :  API for the minimal synchronisation unfolder 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Exploration.SUNF.API where

import Control.Monad.State.Strict
import Data.List
import Data.Map (Map,fromList,empty)
import Domain.Synchron
import Exploration.SUNF.State
import Prelude hiding (succ)
import Util.Generic
import qualified Data.HashTable.IO as H
import qualified Data.Map as MA
import qualified Data.Maybe as M

sep = "-----------------------------------------\n"

-- | Bottom Event
botEID :: Int
botEID = 0

-- API
-- GETTERS 
-- | Retrieves the event associated with the event id 
get_event :: String -> EventID -> Events -> IO Event 
{-# INLINE get_event #-}
get_event s e events = do
  mv <- H.lookup events e 
  case mv of
    Nothing -> do
      str <- showEvents events 
      error $ s ++ "-get_event: " ++ show e ++ "\n" ++ str 
    Just ev -> return ev 

-- | Retrieves fields of an event: immediate sucessors, predecessors, etc.
-- get_pred,get_succ,... :: EventID -> Events -> IO EventsID
get_pred e events = do
  ev@Event{..} <- get_event "getIPred(ecessors)" e events
  return pred 
get_succ e events = do
  ev@Event{..} <- get_event "getISucc(essors)" e events
  return succ 
get_icnf e events = do
  ev@Event{..} <- get_event "getICnfl(icts)" e events
  return icnf 
get_disa e events = do 
  ev@Event{..} <- get_event "getDisa(bled)" e events
  return disa
get_alte e events = do 
  ev@Event{..} <- get_event "getAltrs(natives)" e events
  return alte 
get_name e events = do
  ev@Event{..} <- get_event "getAltrs(natives)" e events
  return name
get_tid e events = do
  ev@Event{..} <- get_event "getAltrs(natives)" e events
  return $ fst name
  
-- SETTERS
set_event :: EventID -> Event -> Events -> IO ()
set_event eID ev events = H.insert events eID ev

-- | delete an event from the event hashtable
del_event :: EventID -> Events -> IO ()
del_event e events = do
  check <- filterEvent e events 
  if check
  then do
    ev@Event{..} <- get_event "deleteEvent" e events
    mapM_ (\e' -> del_succ e e' events) pred
    mapM_ (\e' -> del_icnf e e' events) icnf
    mapM_ (\e' -> del_event e' events) succ
    H.delete events e
  else return ()

add_succ,del_succ,add_icnf,del_icnf,add_disa :: EventID -> EventID -> Events -> IO ()
-- | add e as a sucessor of e'
add_succ e e' events = -- trace ("add_succ: " ++ show e ++ " of " ++ show e') $ 
 do
  ev@Event{..} <- get_event "add_succ" e' events
  let succEv = e:succ
      ev' = ev{ succ = succEv } 
  set_event e' ev' events 

-- | delete e as a successor of e'
del_succ e e' events = -- trace ("setSucc: " ++ show e ++ " of " ++ show e') $ 
 do
  mv <- H.lookup events e' 
  case mv of 
   Nothing -> return ()
   Just ev -> do 
     let succEv = delete e $ succ ev
         ev' = ev{ succ = succEv } 
     set_event e' ev' events 

-- | add e as an immediate conflict of e'
add_icnf e e' events = -- trace ("add_icnf: " ++ show e ++ " of " ++ show e') $ 
 do
  ev@Event{..} <- get_event "add_icnf" e' events
  let icnfEv = e:icnf
      ev' = ev{ icnf = icnfEv }
  set_event e' ev' events 

-- | delete e as an immediate conflict of e' 
del_icnf e e' events = -- trace ("del_icnf: " ++ show e ++ " of " ++ show e') $ 
 do
  ev@Event{..} <- get_event "del_icnf" e' events
  let icnfEv = delete e icnf 
      altEv = filter (\v -> not $ elem e v) alte 
      ev' = ev{ icnf = icnfEv, alte = altEv } 
  set_event e' ev' events 

-- | add e to the disabled set of ê
add_disa e ê events = -- trace ("add_disa: " ++ show e ++ " of " ++ show ê) $ 
 do
  ev@Event{..} <- get_event "add_disa" ê events
  let disaEv = e:disa
      ev' = ev{ disa = disaEv }
  set_event ê ev' events 

-- | set de as the disabled set of e
set_disa :: EventID -> EventsID -> Events -> IO ()
set_disa e de events = -- trace ("setDisa: " ++ show de ++ " of " ++ show e) $ 
 do
  ev@Event{..} <- get_event "set_disa" e events
  let ev' = ev{ disa = de }
  set_event e ev' events 

-- | add v to the alternatives of e
add_alte :: EventID -> Alternative -> Events -> IO ()
add_alte e v events = -- trace ("adding alternative " ++ show v ++ " of " ++ show e) $ 
 do
  ev@Event{..} <- get_event "add_alte" e events
  let altEv = nub $ v:alte
      ev' = ev{ alte = altEv }
  set_event e ev' events 

-- | reset the alternatives of e
reset_alte :: EventID -> Events -> IO ()
reset_alte e events = do
  ev@Event{..} <- get_event "reset_alte" e events
  let altEv = []
      ev' = ev{ alte = altEv }
  set_event e ev' events

-- | set conf as the previous configuration
set_pcnf :: Configuration -> UnfolderOp ()
set_pcnf conf = do
  s@UnfolderState{..} <- get
  let ns = s{ pcnf = conf } 
  put ns

-- Stack related operations
-- @ push 
push :: EventID -> UnfolderOp ()
push e = do 
  s@UnfolderState{..} <- get
  let nstack = e:stak
  put s{ stak = nstack }

-- @ pop
pop :: UnfolderOp ()
pop = do
  s@UnfolderState{..} <- get
  let nstack = tail stak
  put s{ stak = nstack }

-- @ freshCounter - updates the counter of events
freshCounter :: UnfolderOp Counter
freshCounter = do
  s@UnfolderState{..} <- get
  let ec = cntr
      nec = ec + 1
  put s{ cntr = nec }
  return ec

-- Update statistics of the unfolding exploration
inc_max_conf,inc_cutoffs,inc_evs_prefix,dec_evs_prefix,inc_sum_size_max_conf :: UnfolderOp ()
-- | increment nr of maximal configurations
inc_max_conf = do
  s@UnfolderState{..} <- get
  let n_max_conf = nr_max_conf stats + 1
      stats' = stats { nr_max_conf = n_max_conf }
  put s{ stats = stats' }
  
-- | increment nr of cutoffs 
inc_cutoffs = do
  s@UnfolderState{..} <- get
  let n_cutoffs = nr_cutoffs stats + 1
      stats' = stats { nr_cutoffs = n_cutoffs }
  put s{ stats = stats' }

op_evs_prefix :: (Counter -> Counter -> Counter) -> UnfolderOp ()
op_evs_prefix op = do
  s@UnfolderState{..} <- get
  let n_evs_prefix = op (nr_evs_prefix stats) 1
      stats' = stats { nr_evs_prefix = n_evs_prefix }
  put s{ stats = stats' }
-- | increment the size of U
inc_evs_prefix = op_evs_prefix (+)
-- | decrement the size of U
dec_evs_prefix = op_evs_prefix (-)

-- | add to the current size
inc_sum_size_max_conf = do
  s@UnfolderState{..} <- get
  let n_size_max_conf = sum_size_max_conf stats + (toInteger $ nr_evs_prefix stats)
      stats' = stats { sum_size_max_conf = n_size_max_conf }
  put s{ stats = stats' }

-- | increment the table of event names 
inc_evs_per_name :: EventName -> UnfolderOp ()
inc_evs_per_name name = do
  s@UnfolderState{..} <- get  
  let info = nr_evs_per_name stats 
      n_evs_per_name = 
        case MA.lookup name info of
          Nothing -> MA.insert name 1 info 
          Just n  -> MA.insert name (n+1) info 
      stats' = stats { nr_evs_per_name = n_evs_per_name }
  put s{ stats = stats' }

-- | Utility functions
-- | Filters a list of events ids that are still in the prefix 
filterEvents :: EventsID -> Events -> IO EventsID
filterEvents es events = filterM (\e -> filterEvent e events) es

-- | Checks if an event id *e* is still in the prefix
filterEvent :: EventID -> Events -> IO Bool
filterEvent e events = do
  mv <- H.lookup events e
  case mv of
    Nothing -> return False
    Just ev -> return True

-- | Potentially expensive functions
-- | check if e1 >= e2
is_same_or_succ :: EventID -> EventID -> UnfolderOp Bool
is_same_or_succ e1 e2 = undefined 

-- | check if e1 || e2
is_concur :: EventID -> EventID -> UnfolderOp Bool
is_concur e1 e2 = undefined

latest_ev_proc :: EventName -> EventsID -> UnfolderOp EventID
latest_ev_proc = undefined

find_lock_cnfl :: PEvent -> EventID -> UnfolderOp EventID
find_lock_cnfl = undefined

find_prev_unlock :: EventID -> UnfolderOp EventID
find_prev_unlock = undefined

partition_dependent :: PEvent -> Events -> (EventsID,EventsID) -> EventsID -> IO (EventsID,EventsID)
partition_dependent = undefined
{-
-- | Splits between events that are dependent and independent of
-- the event name and actions
partition_dependent :: EventInfo -> Events s -> (EventsID,EventsID) -> EventsID -> ST s (EventsID,EventsID)
partition_dependent êinfo events (dep,indep) es =
  case es of
    [] -> return (dep,indep)
    (e:r) -> do
      ev@Event{..} <- get_event "partition_dependent" e events
      if is_dependent êinfo (name,acts)
      then partition_dependent êinfo events (e:dep,indep) r
      else partition_dependent êinfo events (dep,e:indep) r

is_independent :: EventID -> EventID -> Events s -> ST s Bool
is_independent e1 e2 evts =
  if e1 == GCS.botID || e2 == GCS.botID
  then return False
  else do
    ev1 <- get_event "is_independent" e1 evts
    ev2 <- get_event "is_independent" e1 evts
    return $ not $ is_dependent (name ev1, acts ev1) (name ev2, acts ev2) 

-- | Checks if two event names are dependent
-- This occurs if they are events of the same process
-- or their actions are interfering.
-- Of course, one can emulate events of the same process
-- in their actions (by for example considering Writes to 
-- the PC variable) but this would be more expensive.
is_dependent :: EventInfo -> EventInfo -> Bool
is_dependent a@((pid,_),acts) b@((pid',_),acts') =
--  T.trace ("checking dependency between " ++ show (a,b)) $
-- Missing to check if there is an action which is a create of a pid
  let c1 = GCS.isCreateOf pid' acts  
      c2 = GCS.isCreateOf pid acts' 
  in c1 || c2 || pid == GCS.botID || pid' == GCS.botID || pid == pid' || GCS.interferes acts acts'
-}
-- "UBER" EXPENSIVE OPERATIONS THAT SHOULD BE AVOIDED!
-- predecessors (local configuration) and sucessors of an event
predecessors, successors :: EventID -> Events -> IO EventsID
{-# INLINABLE predecessors #-}
predecessors e events =  do
  preds <- predecessors' e events
  return $ nub preds 
 where
  predecessors' :: EventID -> Events -> IO EventsID
  predecessors' e events = do
     ev@Event{..} <- get_event "predecessors" e events 
     foldM (\a e -> predecessors' e events >>= \r -> return $ a ++ r) pred pred
{-# INLINABLE successors #-}
successors e events =  do 
  succs <- successors' e events
  return $ nub succs 
 where
  successors' :: EventID -> Events -> IO EventsID
  successors' e events = do
     ev@Event{..} <- get_event "successors" e events 
     foldM (\a e -> successors' e events >>= \r -> return $ a ++ r) succ succ

-- | Retrieves all events of a configuration
get_evts_of_conf :: EventsID -> Events -> IO EventsID
get_evts_of_conf maxevs events = do
  preds <- mapM (\e -> predecessors e events) maxevs
  return $ maxevs ++ (nub $ concat preds) 

