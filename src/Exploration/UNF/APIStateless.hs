{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.APIStateless where

import Prelude hiding (succ)

import Control.Monad.State.Strict
import Control.Monad.ST
-- Data Structures
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import qualified Data.Set as S
import Data.Map (Map,fromList,empty)
import qualified Data.Map as MA
import qualified Data.Maybe as M
import qualified Data.ByteString.Char8 as BS
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

-- @ Value of the main HashTable
--   (name, actions, predecessors, successors, #^, D, V)
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

-- @ Bottom event and event_id
botEID :: EventID
botEID = 0

botName :: EventName
botName = undefined

{-
botEvent :: [act] -> Event act
botEvent acts = Event (BS.pack "", GCS.botID, [], acts) [] [] [] [] []

-- @ Initial state of the unfolder
iState :: (Hashable st, Eq st, Ord st, GCS.Projection st) => Bool -> Bool -> GCS.System st -> I.UIndep -> ST s (UnfolderState st s) 
iState statelessMode cutoffMode syst indr = do
  evts <- H.new
  H.insert evts 0 $ botEvent $ GCS.initialActs syst
--  stas <- H.new
  let initialState = GCS.initialState syst
      controlState = GCS.controlPart initialState
      stas = fromList [(controlState,[(initialState, 0)])]
--  H.insert stas initialState botEID
  let pcnf = Conf undefined [] [] [] -- this seems suspicious!
      stak = [botEID]
      cntr = 1
      maxConf = 0
      cutoffCntr = 0
      size = 1
      cumsize = 0
  return $ UnfolderState syst indr evts pcnf stak cntr stas maxConf cutoffCntr size cumsize statelessMode cutoffMode empty

{-
gc :: UnfolderState -> UnfolderState
gc s@UnfolderState{..} = 
  let nevents = 0:(M.foldrWithKey (\e alts r -> nub $ e:(concat alts) ++ r) [] alternatives)
      events' = M.filterWithKey (\eID _ -> eID `elem` nevents) events
      causality' = filter (\(e1,e2) -> e1 `elem` nevents && e2 `elem` nevents) causality
      immediateCnfls = M.filterWithKey (\eID _ -> eID `elem` nevents) immediateConflicts
  in UnfolderState system indep events' configurations causality' enable disable alternatives immediateCnfls counters
-}

-- @ Given the state s and an enabled event e, execute s e
--   is going to apply h(e) to s to produce the new state s'
execute :: st -> EventID -> UnfolderOp st s [st]
{-# INLINE execute #-}
execute cst e = do
  s@UnfolderState{..} <- get
  ev@Event{..} <- lift $ getEvent "execute" e evts 
  let fn = GCS.getTransition syst $ snd4 evtr
  return $ fn cst

isDependent_te :: I.UIndep -> GCS.TransitionInfo -> EventID -> Events s -> ST s Bool
{-# INLINE isDependent_te #-}
isDependent_te indep tr e events = do --trace ("isDependent(tr="++show tr++", e=" ++show e++")")$ 
  ev@Event{..} <- getEvent "isDependent" e events
  return $ I.isDependent indep tr evtr

isIndependent :: I.UIndep -> EventID -> EventID -> Events s -> ST s Bool
{-# INLINE isIndependent #-}
isIndependent indep e ê events = do
  ev <- getEvent "isIndependent" e events 
  êv <- getEvent "isIndependent" ê events
  return $ I.isIndependent indep (evtr ev) (evtr êv) 

-- Useful Functions
predecessors, successors :: EventID -> Events s -> ST s EventsID
{-# INLINABLE predecessors #-}
predecessors e events =  do
  preds <- predecessors' e events
  return $ nub preds 
 where
  predecessors' :: EventID -> Events s -> ST s EventsID
  predecessors' e events = do
     ev@Event{..} <- getEvent "predecessors" e events 
     foldM (\a e -> predecessors' e events >>= \r -> return $ a ++ r) pred pred
{-# INLINABLE successors #-}
successors e events =  do 
  succs <- successors' e events
  return $ nub succs 
 where
  successors' :: EventID -> Events s -> ST s EventsID
  successors' e events = do
     ev@Event{..} <- getEvent "successors" e events 
     foldM (\a e -> successors' e events >>= \r -> return $ a ++ r) succ succ

predecessorWith :: EventID -> GCS.ProcessID -> Events s -> ST s EventID
predecessorWith 0 p events = return GCS.botID
predecessorWith e p events = do
  pred <- getIPred e events
  epred <- filterM (\e -> getEvent "predecessorWith" e events >>= \ev@Event{..} -> return $ fst4 evtr == p) pred
  if null epred 
  then do 
    res <- mapM (\e -> predecessorWith e p events) pred
    return $ filterResult res
  else return $ filterResult epred

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
-- GETTERS
-- retrieves the event associated with the event id 
getEvent :: String -> EventID -> Events s -> ST s Event
{-# INLINE getEvent #-}
getEvent s e events = do
  mv <- H.lookup events e 
  case mv of
    Nothing -> do
      str <- showEvents events 
      error $ s ++ "-getEvent: " ++ show e ++ "\n" ++ str 
    Just ev -> return ev 

filterEvents :: EventsID -> Events s -> ST s EventsID
filterEvents es events = filterM (\e -> filterEvent e events) es

filterEvent :: EventID -> Events s -> ST s Bool
filterEvent e events = do
  mv <- H.lookup events e
  case mv of
    Nothing -> return False
    Just ev -> return True

-- @ getConfEvs - retrieves all the events of a configuration
getConfEvs :: EventsID -> Events s -> ST s EventsID
getConfEvs maxevs events = do
  preds <- mapM (\e -> predecessors e events) maxevs
  return $ maxevs ++ (nub $ concat preds) 
    
getImmediateConflicts :: EventID -> Events s -> ST s EventsID
getImmediateConflicts e events = do
  ev@Event{..} <- getEvent "getImmediateConflicts" e events
  return icnf 

getIPred :: EventID -> Events s -> ST s EventsID
getIPred e events = do
  ev@Event{..} <- getEvent "getIPred" e events
  return pred 

getISucc :: EventID -> Events s -> ST s EventsID
getISucc e events = do
  ev@Event{..} <- getEvent "getISucc" e events
  return succ 

getDisabled :: EventID -> Events s -> ST s EventsID
getDisabled e events = do 
  ev@Event{..} <- getEvent "getIPred" e events
  return disa

getAlternatives :: EventID -> Events s -> ST s Alternatives 
getAlternatives e events = do 
  ev@Event{..} <- getEvent "getAlternatives" e events
  return alte 

-- SETTERS

setEvent :: EventID -> Event -> Events s -> ST s ()
setEvent eID e events = -- trace ("setEvent: " ++ show eID) $ 
  H.insert events eID e

-- @ delImmCnfl 
delImmCnfl :: EventID -> EventID -> Events s -> ST s ()
delImmCnfl e e' events = -- trace ("delImmCnfl: " ++ show e ++ " of " ++ show e') $ 
 do
  ev@Event{..} <- getEvent "setSuccessor" e' events
  let icnfEv = delete e icnf 
      altEv = filter (\v -> not $ elem e v) alte 
      ev' = ev{ icnf = icnfEv, alte = altEv } 
  setEvent e' ev' events 

-- @ delSuccessor e -> e'
delSuccessor :: EventID -> EventID -> Events s -> ST s ()
delSuccessor e e' events = -- trace ("setSucc: " ++ show e ++ " of " ++ show e') $ 
 do
  mv <- H.lookup events e' 
  case mv of 
   Nothing -> return ()
   Just ev -> do 
     let succEv = delete e $ succ ev
         ev' = ev{ succ = succEv } 
     setEvent e' ev' events 

-- @ setSuccessor e -> e'
setSuccessor :: EventID -> EventID -> Events s -> ST s ()
setSuccessor e e' events = -- trace ("setSucc: " ++ show e ++ " of " ++ show e') $ 
 do
  ev@Event{..} <- getEvent "setSuccessor" e' events
  let succEv = e:succ
      ev' = ev{ succ = succEv } 
  setEvent e' ev' events 

setConflict :: EventID -> EventID -> Events s -> ST s ()
setConflict e e' events = -- trace ("setCnfl: " ++ show e ++ " of " ++ show e') $ 
 do
  ev@Event{..} <- getEvent "setConflict" e' events
  let icnfEv = e:icnf
      ev' = ev{ icnf = icnfEv }
  setEvent e' ev' events 

setDisabled :: EventID -> EventsID -> Events s -> ST s ()
setDisabled e de events = -- trace ("setDisa: " ++ show de ++ " of " ++ show e) $ 
 do
  ev@Event{..} <- getEvent "setDisabled" e events
  let ev' = ev{ disa = de }
  setEvent e ev' events 

addAlternative :: EventID -> Alternative -> Events s -> ST s ()
addAlternative e v events = -- trace ("adding alternative " ++ show v ++ " of " ++ show e) $ 
 do
  ev@Event{..} <- getEvent "addAlternative" e events
  let altEv = nub $ v:alte
      ev' = ev{ alte = altEv }
  setEvent e ev' events 

resetAlternatives :: EventID -> Events s -> ST s ()
resetAlternatives e events = do
  ev@Event{..} <- getEvent "resetAlternative" e events
  let altEv = []
      ev' = ev{ alte = altEv }
  setEvent e ev' events

addDisabled :: EventID -> EventID -> Events s -> ST s ()
addDisabled e ê events = -- trace ("addDisa: " ++ show e ++ " of " ++ show ê) $ 
 do
  ev@Event{..} <- getEvent "addDisabled" ê events
  let disaEv = e:disa
      ev' = ev{ disa = disaEv }
  setEvent ê ev' events 
   
-- @ set previous configuration
setPreviousConfiguration :: Configuration st -> UnfolderOp st s ()
setPreviousConfiguration conf = do
  s@UnfolderState{..} <- get
  let ns = s{ pcnf = conf } 
  put ns

-- @ set previous disable set
setPreviousDisabled :: Events s -> UnfolderOp st s ()
setPreviousDisabled events = do
  s@UnfolderState{..} <- get
  kv <- lift $ H.toList events
  lift $ mapM_ (\(e,ev) -> getEvent "setPDisa" e evts >>= \ev' -> 
      let nev = ev' { disa = disa ev }
      in setEvent e nev evts) kv
  return () 

-- @ freshCounter - updates the counter of events
freshCounter :: UnfolderOp st s Counter
freshCounter = do
  s@UnfolderState{..} <- get
  let ec = cntr
      nec = ec + 1
  put s{ cntr = nec }
  return ec

-- @ update maximal config - updates the counter of maximal configurations
incMaxConfCounter :: UnfolderOp st s ()
incMaxConfCounter = do
  s@UnfolderState{..} <- get
  let nec = maxConf + 1
  put s{ maxConf = nec }

-- @ updates the counter of cutoffs
incCutoffCounter :: UnfolderOp st s ()
incCutoffCounter = do
  s@UnfolderState{..} <- get
  let nec = cutoffCntr + 1
  put s{ cutoffCntr = nec }

-- @ increment the size of U
incSize :: UnfolderOp st s ()
incSize = do
  s@UnfolderState{..} <- get
  let nsize = size + 1
  put s{ size = nsize }
  
incSizeTr :: GCS.TransitionInfo -> UnfolderOp st s ()
incSizeTr tr = do
  s@UnfolderState{..} <- get  
  let nevtsPerTrans = case MA.lookup tr evtsPerTrans of
                        Nothing -> MA.insert tr 1 evtsPerTrans
                        Just n  -> MA.insert tr (n+1) evtsPerTrans
  put s{ evtsPerTrans = nevtsPerTrans }

-- @ decrement the size of U
decSize :: UnfolderOp st s ()
decSize = do
  s@UnfolderState{..} <- get
  let nsize = size - 1
  put s{ size = nsize }

-- @ add the current size
incAccSize :: UnfolderOp st s ()
incAccSize = do
  s@UnfolderState{..} <- get
  let ncumsize = cumsize + (toInteger size)
  put s{ cumsize = ncumsize }

-- @ update the cutoff table
updateCutoffTable :: States st -> UnfolderOp st s ()
updateCutoffTable cutoffs = do
  s@UnfolderState{..} <- get
  put s{ stas = cutoffs}

-- @ push 
push :: EventID -> UnfolderOp st s ()
push e = do 
  s@UnfolderState{..} <- get
  let nstack = e:stak
  put s{ stak = nstack }

-- @ pop
pop :: UnfolderOp st s ()
pop = do
  s@UnfolderState{..} <- get
  let nstack = tail stak
  put s{ stak = nstack }

-- @ Util
isConfiguration :: Events s -> EventsID -> ST s Bool
isConfiguration evts conf = do
  cnffree <- allM (\e -> getImmediateConflicts e evts >>= \es -> return $! null (es `intersect` conf)) conf
  causaclosed <- causalClosed evts conf conf 
  return $! cnffree && causaclosed

causalClosed :: Events s -> EventsID -> EventsID ->  ST s Bool
causalClosed evts conf [] = return True
causalClosed evts conf (e:es) = do 
  prede <- predecessors e evts
  if all (\e' -> e' `elem` conf) prede
  then causalClosed evts conf es
  else return False
-}
