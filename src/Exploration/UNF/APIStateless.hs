{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.APIStateless where

import Prelude hiding (succ)

import Control.Monad.State.Strict
import Control.Monad.ST.Safe

-- Data Structures
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import qualified Data.Set as S
import qualified Data.Maybe as M
import qualified Data.ByteString.Char8 as BS
import Data.List
-- import PetriNet

import qualified Model.GCS as GCS

import qualified Debug.Trace as T

mytrace True a b = T.trace a b
mytrace False a b = b

mtrace = T.trace
--trace a b = b
trace a b = b 
--trace = T.trace 

-- @ The most basic type is event_id :: Int
--   Pointer to an event
type EventID = Int
type EventsID = [EventID]

-- @ Configuration  
data Configuration s = Conf {
    stc :: GCS.Sigma s   -- state at this configuration
  , maxevs :: EventsID  -- maximal events of the configuration
  , enevs  :: EventsID  -- enabled events of the configuration
  , cevs   :: EventsID  -- special events: the ones that have imm conflicts
}

-- @ An alternative is a conflicting extension of the configuration
--   that is being/was explored. 
type Alternative = EventsID
type Alternatives = [Alternative]
type Counter = Int

-- @ Value of the main HashTable
--   (transition_id, predecessors, successors, #^, D, V)
data Event = Event {
    evtr :: (GCS.TransitionID, GCS.ProcessID)  -- Transition id
  , pred :: EventsID         -- Immediate predecessors
  , succ :: EventsID         -- Immediate successors
  , icnf :: EventsID         -- Immediate conflicts: #^
  , disa :: EventsID         -- Disabled events: D
  , alte :: Alternatives     -- Valid alternatives: V
  , lcst :: Maybe GCS.LSigma  -- Local state 
} deriving (Show,Eq,Ord)

-- @ Events represents the unfolding prefix as LPES
--   with a HashTable : EventID -> Event 
type Events s = GCS.HashTable s EventID Event

-- @ Show an 
showEvents :: Events s -> ST s String
showEvents evs = do
  m <- H.toList evs
  let km = sort m
      r = foldl (\a m -> show m ++ "\n" ++ a) "" km
      --r = if length km > 54 then show $ [km !! 33, km !! 54] else "" -- foldl (\a m -> show m ++ "\n" ++ a) "" km
  return r

copyEvents :: Events s -> ST s (Events s)
copyEvents s = do
  kv <- H.toList s
  H.fromList kv
 
-- @ The state of the unfolder at any moment
data UnfolderState s = UnfolderState {
    syst :: GCS.System s      -- The system being analyzed
  , inde :: GCS.UIndep        -- Independence relation
  , evts :: Events s         -- Unfolding prefix 
  , pcnf :: Configuration s  -- Previous configuration
  , cntr :: Counter          -- Event counter
  , stack :: EventsID        -- Call stack
  , statelessMode :: Bool    -- Stateless or not
}

-- @ Abbreviation of the type of an operation of the unfolder
type UnfolderOp s a = StateT (UnfolderState s) (ST s) a

-- @ Bottom event and event_id
botEID :: EventID
botEID = 0

botEvent :: GCS.LSigma -> Event
botEvent lst = Event (GCS.botID, BS.pack "") [] [] [] [] [] (Just lst) 

-- @ Initial state of the unfolder
iState :: Bool -> GCS.System s -> GCS.UIndep -> ST s (UnfolderState s) 
iState statelessMode sys indep = do
  events <- H.new
  H.insert events 0 $ botEvent $ GCS.initialLState sys 
  let pconf = Conf undefined [] [] []
  return $ UnfolderState sys indep events pconf 1 [0] statelessMode

beg = "--------------------------------\n BEGIN Unfolder State          \n--------------------------------\n"
end = "\n--------------------------------\n END   Unfolder State          \n--------------------------------\n"
instance Show (UnfolderState s) where
    show (u@UnfolderState{..}) = show cntr 
--        beg ++ "UIndep: " ++ show indep ++ "\nEvents: " ++ show events ++ "\nCausality: " ++ show causality 
--     ++ "\n" ++ show (cevs configurations) ++ "\nEnabled: " ++ show enable 
--     ++ "\nDisable: " ++ show disable ++ "\nAlternatives: " ++ show alternatives  ++ "\nImmConflicts: " ++ show immediateConflicts ++ "\nCounters: " 
--     ++ show counters ++ end

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
execute :: GCS.Sigma s -> EventID -> UnfolderOp s (GCS.Sigma s, GCS.LSigma)
{-# INLINE execute #-}
execute cst e = do
  s@UnfolderState{..} <- get
  ev@Event{..} <- lift $ getEvent "execute" e evts 
  let t = GCS.getTransition syst $ fst evtr
  fn <- lift $ (t cst >>= return . M.fromMaybe (error $ "newState: the transition was not enabled " ++ show cst))
  lift $ fn cst

isDependent_te :: GCS.UIndep -> (GCS.TransitionID, GCS.ProcessID) -> EventID -> Events s -> ST s Bool
{-# INLINE isDependent_te #-}
isDependent_te indep tr e events = do
  ev@Event{..} <- getEvent "isDependent" e events
  return $ GCS.isDependent indep tr evtr

isIndependent :: GCS.UIndep -> EventID -> EventID -> Events s -> ST s Bool
{-# INLINE isIndependent #-}
isIndependent indep e ê events = do
  ev <- getEvent "isIndependent" e events 
  êv <- getEvent "isIndependent" ê events
  return $ GCS.isIndependent indep (evtr ev) (evtr êv) 

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
  epred <- filterM (\e -> getEvent "predecessorWith" e events >>= \ev@Event{..} -> return $ snd evtr == p) pred
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

setLSigma :: EventID -> GCS.LSigma -> Events s -> ST s ()
setLSigma eID st events = -- trace ("setLSigma: " ++ show eID) $ 
 do
  ev@Event{..} <- getEvent "setLSigma" eID events
  let lcst' = case lcst of
        Nothing -> Just st
        Just st' -> if st == st' then Just st else error "setLSigma: different local states"
      ev' = ev{ lcst = lcst' } 
  setEvent eID ev' events 

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

addDisabled :: EventID -> EventID -> Events s -> ST s ()
addDisabled e ê events = -- trace ("addDisa: " ++ show e ++ " of " ++ show ê) $ 
 do
  ev@Event{..} <- getEvent "addDisabled" ê events
  let disaEv = e:disa
      ev' = ev{ disa = disaEv }
  setEvent ê ev' events 
   
-- @ set previous configuration
setPreviousConfiguration :: Configuration s -> UnfolderOp s ()
setPreviousConfiguration conf = do
  s@UnfolderState{..} <- get
  let ns = s{ pcnf = conf } 
  put ns

-- @ set previous disable set
setPreviousDisabled :: Events s -> UnfolderOp s ()
setPreviousDisabled events = do
  s@UnfolderState{..} <- get
  kv <- lift $ H.toList events
  lift $ mapM_ (\(e,ev) -> getEvent "setPDisa" e evts >>= \ev' -> 
      let nev = ev' { disa = disa ev }
      in setEvent e nev evts) kv
  return () 

-- @ freshCounter - updates the counter of events
freshCounter :: UnfolderOp s Counter
freshCounter = do
  s@UnfolderState{..} <- get
  let ec = cntr
      nec = ec + 1
  put s{ cntr = nec }
  return ec

-- @ push 
push :: EventID -> UnfolderOp s ()
push e = do 
  s@UnfolderState{..} <- get
  let nstack = e:stack
  put s{ stack = nstack }

-- @ pop
pop :: UnfolderOp s ()
pop = do
  s@UnfolderState{..} <- get
  let nstack = tail stack
  put s{ stack = nstack }
