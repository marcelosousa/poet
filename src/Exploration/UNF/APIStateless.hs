{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.APIStateless where

import Prelude hiding (succ)

import Control.Monad.State.Strict
import Control.Monad.ST
-- Data Structures
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.Map (Map,fromList,empty)
import qualified Data.Map as MA
import qualified Data.Maybe as M
import Data.List
import Util.Generic

import qualified Model.GCS as GCS
import qualified Debug.Trace as T

mytrace True a b = T.trace a b
mytrace False a b = b

-- @ The most basic type is event_id :: Int
--   Pointer to an event
type EventID = Int
type EventsID = [EventID]
type EventName = (GCS.TId,GCS.Pos)
type EventInfo act = (EventName,[act]) 

-- @ Value of the main HashTable
--   (name, actions, predecessors, successors, #^, D, V)
--  @NOTE: Should actions be part of the name?
data Event act =
  Event 
  {
    name :: EventName    -- ^ Event name 
  , acts :: [act]        -- ^ Event actions 
  , pred :: EventsID     -- ^ Immediate predecessors
  , succ :: EventsID     -- ^ Immediate successors
  , icnf :: EventsID     -- ^ Immediate conflicts: #^
  , disa :: EventsID     -- ^ Disabled events: D
  , alte :: Alternatives -- ^ Valid alternatives: V
  } 
  deriving (Show,Eq,Ord)

-- @ Bottom event: a very special event
botEID :: EventID
botEID = 0
-- The name for bottom
botName :: EventName
botName = (GCS.botID,GCS.botID)
-- The initial bottom event
botEvent :: [act] -> Event act
botEvent acts = Event botName acts [] [] [] [] []

-- @ Events represents the unfolding prefix as LPES
--   with a HashTable : EventID -> Event 
type Events act s = HashTable s EventID (Event act)

-- @ Show the set of events
showEvents :: Show act => Events act s -> ST s String
showEvents evs = do
  m <- H.toList evs
  let km = sortBy (\a b -> compare (fst a) (fst b)) m
      r = foldl (\a m -> show m ++ "\n" ++ a) "" km
  return r

-- @ Counter for various purposes
type Counter = Int

-- @ Configuration  
data Configuration st =
  Conf 
  {
    state :: st        -- ^ state 
  , maevs :: EventsID  -- ^ maximal events 
  , enevs :: EventsID  -- ^ enabled events 
  , cfevs :: EventsID  -- ^ special events: the ones that have imm conflicts
  }

-- @ An history is a set of maximal events of a configuration
-- that enabled an event and each of these maximal events
-- interferes with that event. 
type History = EventsID
type Histories = [History] 

-- @ An alternative is a conflicting extension of a configuration
--   that is being/was explored. 
type Alternative = EventsID
type Alternatives = [Alternative]
    
-- @ HashTable of States to EventID for Cutoffs
--type States s st = HashTable s st EventID
-- Int is the size of the local configuration of that event
type States st = Map st [(st,Int)]

-- @ Options for the unfolder 
data UnfolderOpts =
  UnfOpts
  {
    stateless :: Bool 
  , cutoffs   :: Bool
  }
  deriving (Show,Eq,Ord)

default_unf_opts :: UnfolderOpts
default_unf_opts = UnfOpts False False

-- @ Statistics for the unfolder 
data UnfolderStats =
  UnfStats
  {
    nr_max_conf       :: Counter  -- Nr of maximal configurations 
  , nr_cutoffs        :: Counter  -- Nr of cutoffs
  , nr_evs_prefix     :: Counter  -- Size of prefix |U|
  , sum_size_max_conf :: Integer  -- Sum of sizes of maximal configurations
  , nr_evs_per_name   :: Map EventName Int -- Number of events per name 
  }
  deriving (Show,Eq,Ord)

default_unf_stats :: UnfolderStats
default_unf_stats =
 let nr_max_conf = 0
     nr_cutoffs = 0
     nr_evs_prefix = 1
     sum_size_max_conf = 0
     nr_evs_per_name = MA.singleton botName 1
 in UnfStats nr_max_conf nr_cutoffs nr_evs_prefix
             sum_size_max_conf nr_evs_per_name

-- @ The state of the unfolder at any moment
data UnfolderState st act s = 
  UnfolderState
  {
    syst :: GCS.System st act -- ^ The system being analyzed
  , evts :: Events act s      -- ^ Unfolding prefix 
  , pcnf :: Configuration st  -- ^ Previous configuration
  , stak :: EventsID          -- ^ Call stack
  , cntr :: Counter           -- ^ Event counter
  , stas :: States st         -- ^ Hash Table for cutoffs
  , opts :: UnfolderOpts      -- ^ Options
  , stats :: UnfolderStats    -- ^ Statistics 
}

-- @ Initial state of the unfolder
i_unf_state:: (Hashable st, Ord st, GCS.Collapsible st act) => Bool -> Bool -> GCS.System st act -> ST s (UnfolderState st act s) 
i_unf_state stateless_mode cutoff_mode syst = do
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
      opts = UnfOpts stateless_mode cutoff_mode
  return $ UnfolderState syst evts pcnf stak cntr stas opts default_unf_stats 

-- | Print the state of the unfolder
beg = "--------------------------------\n BEGIN Unfolder State          \n--------------------------------\n"
end = "\n--------------------------------\n END   Unfolder State          \n--------------------------------\n"
instance Show (UnfolderState st act s) where
    show (u@UnfolderState{..}) = show stats 
--        beg ++ "UIndep: " ++ show indep ++ "\nEvents: " ++ show events ++ "\nCausality: " ++ show causality 
--     ++ "\n" ++ show (cevs configurations) ++ "\nEnabled: " ++ show enable 
--     ++ "\nDisable: " ++ show disable ++ "\nAlternatives: " ++ show alternatives  ++ "\nImmConflicts: " ++ show immediateConflicts ++ "\nCounters: " 
--     ++ show counters ++ end

-- @ Abbreviation of the type of an operation of the unfolder
type UnfolderOp st act s a = StateT (UnfolderState st act s) (ST s) a

{-
gc :: UnfolderState -> UnfolderState
gc s@UnfolderState{..} = 
  let nevents = 0:(M.foldrWithKey (\e alts r -> nub $ e:(concat alts) ++ r) [] alternatives)
      events' = M.filterWithKey (\eID _ -> eID `elem` nevents) events
      causality' = filter (\(e1,e2) -> e1 `elem` nevents && e2 `elem` nevents) causality
      immediateCnfls = M.filterWithKey (\eID _ -> eID `elem` nevents) immediateConflicts
  in UnfolderState system indep events' configurations causality' enable disable alternatives immediateCnfls counters

-- @ Given the state s and an enabled event e, execute s e
--   is going to apply h(e) to s to produce the new state s'
execute :: st -> EventID -> UnfolderOp st act s [st]
{-# INLINE execute #-}
execute cst e = do
  s@UnfolderState{..} <- get
  ev@Event{..} <- lift $ get_event "execute" e evts 
  let fn = GCS.getTransition syst $ snd4 evtr
  return $ fn cst

isDependent_te :: I.UIndep -> GCS.TransitionInfo -> EventID -> Events act s -> ST s Bool
{-# INLINE isDependent_te #-}
isDependent_te indep tr e events = do --trace ("isDependent(tr="++show tr++", e=" ++show e++")")$ 
  ev@Event{..} <- get_event "isDependent" e events
  return $ I.isDependent indep tr evtr

isIndependent :: I.UIndep -> EventID -> EventID -> Events act s -> ST s Bool
{-# INLINE isIndependent #-}
isIndependent indep e ê events = do
  ev <- get_event "isIndependent" e events 
  êv <- get_event "isIndependent" ê events
  return $ I.isIndependent indep (evtr ev) (evtr êv) 
-}

-- API
-- GETTERS 
-- | Retrieves the event associated with the event id 
get_event :: Show act => String -> EventID -> Events act s -> ST s (Event act)
{-# INLINE get_event #-}
get_event s e events = do
  mv <- H.lookup events e 
  case mv of
    Nothing -> do
      str <- showEvents events 
      error $ s ++ "-get_event: " ++ show e ++ "\n" ++ str 
    Just ev -> return ev 

-- | Retrieves fields of an event: immediate sucessors, predecessors, etc.
-- get_pred,get_succ,... :: EventID -> Events act s -> ST s EventsID
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

-- SETTERS
set_event :: EventID -> Event act -> Events act s -> ST s ()
set_event eID ev events = H.insert events eID ev

-- | delete an event from the event hashtable
del_event :: Show act => EventID -> Events act s -> ST s ()
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

add_succ,del_succ,add_icnf,del_icnf,add_disa :: Show act => EventID -> EventID -> Events act s -> ST s ()
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
set_disa :: Show act => EventID -> EventsID -> Events act s -> ST s ()
set_disa e de events = -- trace ("setDisa: " ++ show de ++ " of " ++ show e) $ 
 do
  ev@Event{..} <- get_event "set_disa" e events
  let ev' = ev{ disa = de }
  set_event e ev' events 

-- | add v to the alternatives of e
add_alte :: Show act => EventID -> Alternative -> Events act s -> ST s ()
add_alte e v events = -- trace ("adding alternative " ++ show v ++ " of " ++ show e) $ 
 do
  ev@Event{..} <- get_event "add_alte" e events
  let altEv = nub $ v:alte
      ev' = ev{ alte = altEv }
  set_event e ev' events 

-- | reset the alternatives of e
reset_alte :: Show act => EventID -> Events act s -> ST s ()
reset_alte e events = do
  ev@Event{..} <- get_event "reset_alte" e events
  let altEv = []
      ev' = ev{ alte = altEv }
  set_event e ev' events

-- | set conf as the previous configuration
set_pcnf :: Configuration st -> UnfolderOp st act s ()
set_pcnf conf = do
  s@UnfolderState{..} <- get
  let ns = s{ pcnf = conf } 
  put ns

-- Stack related operations
-- @ push 
push :: EventID -> UnfolderOp st act s ()
push e = do 
  s@UnfolderState{..} <- get
  let nstack = e:stak
  put s{ stak = nstack }

-- @ pop
pop :: UnfolderOp st act s ()
pop = do
  s@UnfolderState{..} <- get
  let nstack = tail stak
  put s{ stak = nstack }

-- @ freshCounter - updates the counter of events
freshCounter :: UnfolderOp st act s Counter
freshCounter = do
  s@UnfolderState{..} <- get
  let ec = cntr
      nec = ec + 1
  put s{ cntr = nec }
  return ec

-- | set (update) the cutoff table
set_cutoff_table :: States st -> UnfolderOp st act s ()
set_cutoff_table cutoffs = do
  s@UnfolderState{..} <- get
  put s{ stas = cutoffs}

-- Update statistics of the unfolding exploration
inc_max_conf,inc_cutoffs,inc_evs_prefix,dec_evs_prefix,inc_sum_size_max_conf :: UnfolderOp st act s ()
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

op_evs_prefix :: (Counter -> Counter -> Counter) -> UnfolderOp st act s ()
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
inc_evs_per_name :: EventName -> UnfolderOp st act s ()
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
filterEvents :: EventsID -> Events act s -> ST s EventsID
filterEvents es events = filterM (\e -> filterEvent e events) es

-- | Checks if an event id *e* is still in the prefix
filterEvent :: EventID -> Events act s -> ST s Bool
filterEvent e events = do
  mv <- H.lookup events e
  case mv of
    Nothing -> return False
    Just ev -> return True

-- | Splits between events that are dependent and independent of
-- the event name and actions
partition_dependent :: (Show act, GCS.Action act) => (EventName,[act]) -> Events act s -> (EventsID,EventsID) -> EventsID -> ST s (EventsID,EventsID)
partition_dependent êinfo events (dep,indep) es =
  case es of
    [] -> return (dep,indep)
    (e:r) -> do
      ev@Event{..} <- get_event "partition_dependent" e events
      if is_dependent êinfo (name,acts)
      then partition_dependent êinfo events (e:dep,indep) r
      else partition_dependent êinfo events (dep,e:indep) r

-- | Checks if two event names are dependent
-- This occurs if they are events of the same process
-- or their actions are interfering.
-- Of course, one can emulate events of the same process
-- in their actions (by for example considering Writes to 
-- the PC variable) but this would be more expensive.
is_dependent :: GCS.Action act => (EventName,[act]) -> (EventName,[act]) -> Bool
is_dependent ((pid,_),acts) ((pid',_),acts') =
  pid == pid' || GCS.interferes acts acts'

-- "UBER" EXPENSIVE OPERATIONS THAT SHOULD BE AVOIDED!
-- predecessors (local configuration) and sucessors of an event
predecessors, successors :: Show act => EventID -> Events act s -> ST s EventsID
{-# INLINABLE predecessors #-}
predecessors e events =  do
  preds <- predecessors' e events
  return $ nub preds 
 where
  predecessors' :: Show act => EventID -> Events act s -> ST s EventsID
  predecessors' e events = do
     ev@Event{..} <- get_event "predecessors" e events 
     foldM (\a e -> predecessors' e events >>= \r -> return $ a ++ r) pred pred
{-# INLINABLE successors #-}
successors e events =  do 
  succs <- successors' e events
  return $ nub succs 
 where
  successors' :: Show act => EventID -> Events act s -> ST s EventsID
  successors' e events = do
     ev@Event{..} <- get_event "successors" e events 
     foldM (\a e -> successors' e events >>= \r -> return $ a ++ r) succ succ

-- | Retrieves all events of a configuration
get_evts_of_conf :: Show act => EventsID -> Events act s -> ST s EventsID
get_evts_of_conf maxevs events = do
  preds <- mapM (\e -> predecessors e events) maxevs
  return $ maxevs ++ (nub $ concat preds) 

-- @OBSOLETE (TO BE REMOVED IN THE NEXT VERSION)
-- | restore previous disable set
set_previous_disa :: Show act => Events act s -> UnfolderOp st act s ()
set_previous_disa events = do
  s@UnfolderState{..} <- get
  kv <- lift $ H.toList events
  lift $ mapM_ (\(e,ev) -> get_event "setPDisa" e evts >>= \ev' -> 
      let nev = ev' { disa = disa ev }
      in set_event e nev evts) kv
  return () 

is_configuration :: Show act => Events act s -> EventsID -> ST s Bool
is_configuration evts conf = do
  cnffree <- allM (\e -> get_icnf e evts >>= \es -> return $! null (es `intersect` conf)) conf
  causaclosed <- is_causally_closed evts conf conf 
  return $! cnffree && causaclosed

is_causally_closed :: Show act => Events act s -> EventsID -> EventsID ->  ST s Bool
is_causally_closed evts conf [] = return True
is_causally_closed evts conf (e:es) = do 
  prede <- predecessors e evts
  if all (\e' -> e' `elem` conf) prede
  then is_causally_closed evts conf es
  else return False

predecessorWith :: Show act => EventID -> EventName -> Events act s -> ST s EventID
predecessorWith 0 p events = return GCS.botID
predecessorWith e p events = do
  pred <- get_pred e events
  epred <- filterM (\e -> get_event "predecessorWith" e events >>= \ev@Event{..} -> return $ name == p) pred
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
