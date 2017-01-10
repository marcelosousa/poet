{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.API where

import Control.Monad.ST
import Control.Monad.State.Strict
import Data.List
import Data.Map (Map,fromList,empty)
import Exploration.UNF.State
import Language.SimpleC.AST
import Language.SimpleC.Flow
import Prelude hiding (succ)
import Util.Generic
import qualified Data.HashTable.IO as H
import qualified Data.Map as MA
import qualified Data.Maybe as M
import qualified Debug.Trace as T
import qualified Model.GCS as GCS

-- | Default values for various types
-- @ Bottom event: a very special event
botEID :: EventID
botEID = 0

-- The name for bottom
botName :: EventName
botName = (GCS.botID, GCS.botID, SymId (-1))

-- The initial bottom event
botEvent :: act -> Event act
botEvent acts = Event botName acts [] [] [] [] []

default_unf_opts :: UnfolderOpts
default_unf_opts = UnfOpts False False

default_unf_stats :: UnfolderStats
default_unf_stats =
 let nr_max_conf = 0
     nr_cutoffs = 0
     nr_evs_prefix = 1
     sum_size_max_conf = 0
     nr_evs_per_name = MA.singleton botName 1
 in UnfStats nr_max_conf nr_cutoffs nr_evs_prefix
             sum_size_max_conf nr_evs_per_name

-- @ Initial state of the unfolder
i_unf_state :: GCS.Collapsible st a => Bool -> Bool -> GCS.System st a -> IO (UnfolderState st a)
i_unf_state stl cut syst = do
  evts <- H.new
  H.insert evts botEID $ botEvent $ GCS.gbac syst
  let initialState = GCS.gbst syst
      controlState = GCS.controlPart initialState
      stas = fromList [(controlState,[(initialState,0)])]
--  stas <- H.new
--  H.insert stas initialState botEID
  let pcnf = Conf undefined [] [] [] -- @NOTE: Check! 
      stak = [botEID]
      cntr = 1
      opts = UnfOpts stl cut 
  return $ UnfolderState syst evts pcnf stak cntr stas opts default_unf_stats 

-- API
-- GETTERS 
-- | Retrieves the event associated with the event id 
get_event :: Show act => String -> EventID -> Events act -> IO (Event act)
{-# INLINE get_event #-}
get_event s e events = do
  mv <- H.lookup events e 
  case mv of
    Nothing -> do
      str <- showEvents events 
      error $ s ++ "-get_event: " ++ show e ++ "\n" ++ str 
    Just ev -> return ev 

-- | Retrieves fields of an event: immediate sucessors, predecessors, etc.
-- get_pred,get_succ,... :: EventID -> Events act -> IO EventsID
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
  return $ fst3 name
get_tid_sym e events = do
  ev@Event{..} <- get_event "getAltrs(natives)" e events
  return $ trd3 name
  
-- SETTERS
set_event :: EventID -> Event act -> Events act -> IO ()
set_event eID ev events = H.insert events eID ev

-- | delete an event from the event hashtable
del_event :: Show act => EventID -> Events act -> IO ()
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

add_succ, del_succ, add_icnf, del_icnf :: Show act => EventID -> EventID -> Events act -> IO ()
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
add_disa :: Show act => EventID -> EventID -> Events act -> IO ()
add_disa e ê events = -- trace ("add_disa: " ++ show e ++ " of " ++ show ê) $ 
 do
  ev@Event{..} <- get_event "add_disa" ê events
  let disaEv = e:disa
      ev' = ev{ disa = disaEv }
  set_event ê ev' events 

-- | set de as the disabled set of e
set_disa :: Show act => EventID -> EventsID -> Events act -> IO ()
set_disa e de events = -- trace ("setDisa: " ++ show de ++ " of " ++ show e) $ 
 do
  ev@Event{..} <- get_event "set_disa" e events
  let ev' = ev{ disa = de }
  set_event e ev' events 

-- | add v to the alternatives of e
add_alte :: Show act => EventID -> Alternative -> Events act -> IO ()
add_alte e v events = -- trace ("adding alternative " ++ show v ++ " of " ++ show e) $ 
 do
  ev@Event{..} <- get_event "add_alte" e events
  let altEv = nub $ v:alte
      ev' = ev{ alte = altEv }
  set_event e ev' events 

-- | reset the alternatives of e
reset_alte :: Show act => EventID -> Events act -> IO ()
reset_alte e events = do
  ev@Event{..} <- get_event "reset_alte" e events
  let altEv = []
      ev' = ev{ alte = altEv }
  set_event e ev' events

-- | set conf as the previous configuration
set_pcnf :: Configuration st -> UnfolderOp st act ()
set_pcnf conf = do
  s@UnfolderState{..} <- get
  let ns = s{ pcnf = conf } 
  put ns

-- Stack related operations
-- @ push 
push :: EventID -> UnfolderOp st act ()
push e = do 
  s@UnfolderState{..} <- get
  let nstack = e:stak
  put s{ stak = nstack }

-- @ pop
pop :: UnfolderOp st act ()
pop = do
  s@UnfolderState{..} <- get
  let nstack = tail stak
  put s{ stak = nstack }

-- @ freshCounter - updates the counter of events
freshCounter :: UnfolderOp st act Counter
freshCounter = do
  s@UnfolderState{..} <- get
  let ec = cntr
      nec = ec + 1
  put s{ cntr = nec }
  return ec

-- | set (update) the cutoff table
set_cutoff_table :: States st -> UnfolderOp st act ()
set_cutoff_table cutoffs = do
  s@UnfolderState{..} <- get
  put s{ stas = cutoffs}

-- Update statistics of the unfolding exploration
inc_max_conf, inc_cutoffs, inc_evs_prefix, dec_evs_prefix :: UnfolderOp st act ()
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

op_evs_prefix :: (Counter -> Counter -> Counter) -> UnfolderOp st act ()
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
inc_sum_size_max_conf :: UnfolderOp st act ()
inc_sum_size_max_conf = do
  s@UnfolderState{..} <- get
  let n_size_max_conf = sum_size_max_conf stats + (toInteger $ nr_evs_prefix stats)
      stats' = stats { sum_size_max_conf = n_size_max_conf }
  put s{ stats = stats' }

-- | increment the table of event names 
inc_evs_per_name :: EventName -> UnfolderOp st act ()
inc_evs_per_name name = do
  s@UnfolderState{..} <- get  
  let info = nr_evs_per_name stats 
      n_evs_per_name = 
        case MA.lookup name info of
          Nothing -> MA.insert name 1 info 
          Just n  -> MA.insert name (n+1) info 
      stats' = stats { nr_evs_per_name = n_evs_per_name }
  put s{ stats = stats' }

{-
inc_widen_map :: EventName -> UnfolderOp st act ()
inc_widen_map ename = do 
  s@UnfolderState{..} <- get
  let ewide' = case MA.lookup ename ewide of
        Nothing -> MA.insert ename 1 ewide
        Just n  -> MA.insert ename (n+1) ewide
  put s { ewide = ewide' }
 
set_widen_map :: Map NodeId Int -> UnfolderOp st act ()
set_widen_map wmap = do 
  s@UnfolderState{..} <- get
  put s{ widen = wmap }
-}

-- | Utility functions
-- | Filters a list of events ids that are still in the prefix 
filterEvents :: EventsID -> Events act -> IO EventsID
filterEvents es events = filterM (\e -> filterEvent e events) es

-- | Checks if an event id *e* is still in the prefix
filterEvent :: EventID -> Events act -> IO Bool
filterEvent e events = do
  mv <- H.lookup events e
  case mv of
    Nothing -> return False
    Just ev -> return True

-- | Splits between events that are dependent and independent of
-- the event name and actions
partition_dependent :: (Show act, GCS.Action act) => EventInfo act -> Events act -> (EventsID, EventsID) -> EventsID -> IO (EventsID, EventsID)
partition_dependent êinfo events (dep,indep) es = do
  case es of
    [] -> do
      putStrLn "partition_dependent: end" 
      return (dep,indep)
    (e:r) -> do
      ev@Event{..} <- get_event "partition_dependent" e events
      let is_dep = is_dependent êinfo (name,acts)
      putStrLn $ "\t e = " ++ show e ++ ", result = " ++ show is_dep  
      putStrLn $ "\t name = " ++ show name 
      putStrLn $ "\t acts = " ++ show acts 
      if is_dep 
      then partition_dependent êinfo events (e:dep,indep) r
      else partition_dependent êinfo events (dep,e:indep) r

is_independent :: (Show act, GCS.Action act) => EventID -> EventID -> Events act -> IO Bool
is_independent e1 e2 evts =
  if e1 == GCS.botID || e2 == GCS.botID
  then return False
  else do
    ev1 <- get_event "is_independent" e1 evts
    ev2 <- get_event "is_independent" e2 evts
    return $ not $ is_dependent (name ev1, acts ev1) (name ev2, acts ev2) 

-- | Checks if two event names are dependent
-- This occurs if they are events of the same process
-- or their actions are interfering.
-- Of course, one can emulate events of the same process
-- in their actions (by for example considering Writes to 
-- the PC variable) but this would be more expensive.
is_dependent :: (Show act, GCS.Action act) => EventInfo act -> EventInfo act -> Bool
is_dependent a@((pid,_,tid),acts) b@((pid',_,tid'),acts') = 
  let c1 = pid == GCS.botID || pid' == GCS.botID
      c2 = pid == pid' 
      c3 = GCS.interferes acts acts'
      c4 = GCS.isCreateOf (SymId pid) acts' 
      c5 = GCS.isCreateOf (SymId pid') acts
      r  = or [c1,c2,c3,c4,c5] 
  in r

-- "UBER" EXPENSIVE OPERATIONS THAT SHOULD BE AVOIDED!
-- predecessors (local configuration) and sucessors of an event
predecessors, successors :: Show act => EventID -> Events act -> IO EventsID
{-# INLINABLE predecessors #-}
predecessors e events =  do
  preds <- predecessors' e events
  return $ nub preds 
 where
  predecessors' :: Show act => EventID -> Events act -> IO EventsID
  predecessors' e events = do
     ev@Event{..} <- get_event "predecessors" e events 
     foldM (\a e -> predecessors' e events >>= \r -> return $ a ++ r) pred pred
{-# INLINABLE successors #-}
successors e events =  do 
  succs <- successors' e events
  return $ nub succs 
 where
  successors' :: Show act => EventID -> Events act -> IO EventsID
  successors' e events = do
     ev@Event{..} <- get_event "successors" e events 
     foldM (\a e -> successors' e events >>= \r -> return $ a ++ r) succ succ

-- | Retrieves all events of a configuration
get_evts_of_conf :: Show act => EventsID -> Events act -> IO EventsID
get_evts_of_conf maxevs events = do
  preds <- mapM (\e -> predecessors e events) maxevs
  return $ maxevs ++ (nub $ concat preds) 

-- @OBSOLETE (TO BE REMOVED IN THE NEXT VERSION)
-- | restore previous disable set
set_previous_disa :: Show act => Events act -> UnfolderOp st act ()
set_previous_disa events = do
  s@UnfolderState{..} <- get
  kv <- lift $ H.toList events
  lift $ mapM_ (\(e,ev) -> get_event "setPDisa" e evts >>= \ev' -> 
      let nev = ev' { disa = disa ev }
      in set_event e nev evts) kv
  return () 

is_configuration :: Show act => Events act -> EventsID -> IO Bool
is_configuration evts conf = do
  cnffree <- allM (\e -> get_icnf e evts >>= \es -> return $! null (es `intersect` conf)) conf
  causaclosed <- is_causally_closed evts conf conf 
  return $! cnffree && causaclosed

is_causally_closed :: Show act => Events act -> EventsID -> EventsID -> IO Bool
is_causally_closed evts conf [] = return True
is_causally_closed evts conf (e:es) = do 
  prede <- predecessors e evts
  if all (\e' -> e' `elem` conf) prede
  then is_causally_closed evts conf es
  else return False

predecessorWith :: Show act => EventID -> EventName -> Events act -> IO EventID
predecessorWith 0 p events = return GCS.botID
predecessorWith e p events = do
  pred <- get_pred e events
  epred <- filterM (\e -> get_event "predecessorWith" e events >>= 
                    \ev@Event{..} -> return $ name == p) pred
  if null epred 
  then do 
    res <- mapM (\e -> predecessorWith e p events) pred
    return $ filterResult res
  else return $ filterResult epred
 where 
   filterResult :: EventsID -> EventID 
   filterResult es = 
     if null es 
     then error "predecessorWith: shouldn't happen"
     else let res = filter (/= 0) es
          in if null res
             then GCS.botID
             else if all (== (head res)) (tail res)
                  then head res
                  else error "predecessorWith: multiple possibilities"
