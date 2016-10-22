{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Exploration.SUNF.API
-- Desc      :  API for the minimal synchronisation unfolder 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Exploration.SUNF.API where

import Control.Monad.State.Strict hiding (state)
import Data.List
import Data.Map (Map,fromList,empty)
import Domain.Synchron
import Exploration.SUNF.State hiding (state)
import Haskroid.Hapiroid (ActType(..), Action(..))
import Prelude hiding (succ)
import Util.Generic
import qualified Data.HashTable.IO as H
import qualified Data.Map as MA
import qualified Data.Maybe as M

sep = "-----------------------------------------\n"

-- | Bottom Event
botEID :: Int
botEID = (-1)

botEvent :: Event
botEvent = Event (-1, 0) [] [] [] [] [] (Act ENTRY (-1) (-1)) 

-- @ Initial state of the unfolder
i_unf_state:: Bool -> Bool -> System -> IO UnfolderState 
i_unf_state stl cut syst = do
  evts <- H.new
  H.insert evts botEID botEvent 
  let pcnf = Conf (state syst) [botEID] (MA.fromList [(-1,botEID)]) [] []  
      stak = [botEID]
      cntr = 1
      opts = UnfOpts stl cut
  return $ UnfolderState syst evts pcnf stak cntr opts default_unf_stats 

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

-- | set new system 
set_sys :: System -> UnfolderOp ()
set_sys sys = do
  s@UnfolderState{..} <- get
  let ns = s{ syst = sys } 
  put ns

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

get_events :: String -> EventsID -> UnfolderOp [(EventID, Event)]
get_events str eIDs = do
  s@UnfolderState{..} <- get
  lift $ mapM (\e -> get_event "exit_ev" e evts >>= \ev -> return (e,ev)) eIDs

is_of_tid :: TId -> Event -> Bool
is_of_tid tid e@Event{..} = tid == (fst name) 

-- | traverse evs up searching for the event EXIT of tid
exit_ev :: Integer -> EventsID -> UnfolderOp EventID
exit_ev tid []   = error $ "couldn't find the exit event of th " ++ show tid
exit_ev tid eIDs = do
  s@UnfolderState{..} <- get
  evs <- get_events "exit_ev" eIDs   
  case filter (\(_,e) -> is_of_tid tid e) evs of
    []      -> do
      -- get all immediate predecessors of eIDs
      _preds <- lift $ mapM (\e -> get_pred e evts) eIDs
      let preds = nub $ concat _preds
      exit_ev tid preds 
    [(e,_)] -> return e -- as the exit event is the last one 

compute_hist :: EventID -> EventID -> UnfolderOp History
compute_hist e1 e2 = do
  c1 <- is_same_or_succ e1 e2 
  if c1 
  then do
    lift $ putStrLn $ "compute_hist " ++ show (e1,e2) ++ " is " ++ show [e1]
    return [e1]
  else do
    c2 <- is_pred e1 e2 
    if c2
    then do
      lift $ putStrLn $ "compute_hist " ++ show (e1,e2) ++ " is " ++ show [e2]
      return [e2]
    else do
      lift $ putStrLn $ "compute_hist " ++ show (e1,e2) ++ " is " ++ show [e1,e2]
      return [e1, e2]

is_unlock_of :: Integer -> EventID -> UnfolderOp Bool 
is_unlock_of addr eID = do
  s@UnfolderState{..} <- get
  e@Event{..} <- lift $ get_event "is_unlock_of" eID evts
  return $ is_unlock_act_of acts addr 

is_begin_of :: TId -> EventID -> UnfolderOp Bool 
is_begin_of tid eID = do
  s@UnfolderState{..} <- get
  e@Event{..} <- lift $ get_event "is_begin_of" eID evts
  return $ is_begin_act_of acts (fst name) tid 

is_lock_of :: Integer -> EventID -> UnfolderOp Bool 
is_lock_of addr eID = do
  s@UnfolderState{..} <- get
  e@Event{..} <- lift $ get_event "is_lock_of" eID evts
  return $ is_lock_act_of acts addr
 
-- | Potentially expensive functions
-- | check if e1 >= e2
is_same_or_succ :: EventID -> EventID -> UnfolderOp Bool
is_same_or_succ e1 e2
  | e2 == botEID = return True 
  | e1 == e2     = return True
  | e1 == botEID = return False 
  | e1 < e2      = return False 
  | otherwise = do
      -- check if they are of the same process
      s@UnfolderState{..} <- get
      ev1 <- lift $ get_event "is_same_or_succ" e1 evts
      ev2 <- lift $ get_event "is_same_or_succ" e2 evts
      let (th1,i1) = name ev1
          (th2,i2) = name ev2
      if th1 == th2
      then return $ i1 >= i2 
      else do 
        -- they are in different threads
        preds <- lift $ predecessors e1 evts
        return $ elem e2 preds 

-- | Check if e1 < e2
is_pred :: EventID -> EventID -> UnfolderOp Bool
is_pred e1 e2 
  | e1 == botEID = return True
  | e2 == botEID = return False 
  | e1 >= e2     = return False 
  | otherwise = do
      -- check if they are of the same process
      s@UnfolderState{..} <- get
      ev1 <- lift $ get_event "is_pred" e1 evts
      ev2 <- lift $ get_event "is_pred" e2 evts
      let (th1,i1) = name ev1
          (th2,i2) = name ev2
      if th1 == th2
      then return $ i1 < i2 
      else do
        -- they are in different threads
        preds <- lift $ predecessors e2 evts
        return $ elem e1 preds 

-- | Find all unlocks
--    Given a linearization it will reverse it
--    and filter all unlocks of a given addr
--  This is expensive for now.
unlocks_of_addr :: TId -> Integer -> EventsID -> UnfolderOp EventsID
unlocks_of_addr tid addr stak = do
  lift $ putStrLn $ "unlocks_of_addr: " ++ show (addr,tid) ++ " " ++ show stak
  res <- foldM (unlock_of_addr addr) [] stak
  let rres = reverse res
  lift $ putStrLn $ "unlocks_of_addr: result " ++ show rres
  return $ rres

unlock_of_addr :: Integer -> EventsID -> EventID -> UnfolderOp EventsID
unlock_of_addr addr unlks e = do
  b <- is_unlock_of addr e
  c <- is_lock_of addr e
  if b || c
  then return $ e:unlks
  else return unlks  

latest_ev_proc :: TId -> MEventsID -> UnfolderOp EventID
latest_ev_proc (-1) maxes = return botEID 
latest_ev_proc _tid maxes = do
  let tid = fromInteger _tid 
  case MA.lookup tid maxes of
    Nothing -> return botEID -- error $ "latest_ev_proc: key not found " ++ show tid
    Just e  -> return e 

find_lock_cnfl :: PEvent -> EventID -> UnfolderOp EventID
find_lock_cnfl lk_pe e_unlk = do
  s@UnfolderState{..} <- get
  if is_lock lk_pe
  then do
    es  <- lift $ get_succ e_unlk evts
    evs <- get_events "find_lock_cnfl" es 
    case filter (\(_,e) -> is_lock_act_of (acts e) (lock_addr lk_pe)) evs of
      [] -> error "find_lock_cnfl: broken assumption"
      [(e,_)] -> return e 
  else error "find_lock_cnfl: input pe is not a lock" 


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

