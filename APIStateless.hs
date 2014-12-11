{-#LANGUAGE RecordWildCards #-}
module APIStateless where

import Control.Monad.State.Strict
import Control.Monad.ST.Safe

import Data.List
import Data.Maybe
import qualified Data.Map as M

import qualified Model as ML

import Debug.Trace 

type EventsID = [EventID]
type EventID = Int

type Causality = [(EventID, EventID)]
type Conflict = [(EventID, EventID)]

data Event = Event {
    etr :: ML.TransitionID
--  , econf :: ConfigurationID
} 
  deriving (Show,Eq,Ord)
  
type ConfigurationID = Int
data Configuration s = Configuration {
    cevs :: EventsID
  , cst :: ML.Sigma s
}

type Alternative = EventsID
type Alternatives = [Alternative]
type EventCounter = Int
type ConfigurationCounter = Int
type Counter = Int

type Events = M.Map EventID Event

data UnfolderState s = 
    UnfolderState {
      system :: ML.System s                                     -- The system being analyzed
    , indep :: ML.UIndep                                        -- Independence relation
    , events :: M.Map EventID Event                          -- Visible events of the unfolding prefix
    , configurations :: Configuration s -- M.Map ConfigurationID Configuration  -- ConfigurationID -> {Events that form the conf} !Can be removed?
    , causality :: Causality                            -- Causality
    , enable  :: M.Map ConfigurationID EventsID         -- Enabled events at a configuration
    , disable :: M.Map EventID EventsID                 -- D
    , alternatives :: M.Map EventID Alternatives        -- V
    , immediateConflicts :: M.Map EventID EventsID      -- #^
    , counters :: (EventCounter,ConfigurationCounter)
    }

type UnfolderOp s a = StateT (UnfolderState s) (ST s) a

initialState :: ML.System s -> ML.UIndep -> UnfolderState s
initialState sys indep = 
    UnfolderState sys indep (M.singleton 0 $ Event ML.botID) (Configuration [] undefined) [] M.empty M.empty M.empty M.empty (1,0)
   -- UnfolderState sys indep (M.singleton 0 $ Event "bot") M.empty [] M.empty M.empty M.empty M.empty (1,0)

beg = "--------------------------------\n BEGIN Unfolder State          \n--------------------------------\n"
end = "\n--------------------------------\n END   Unfolder State          \n--------------------------------\n"
instance Show (UnfolderState s) where
    show (u@UnfolderState{..}) = "" 
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

newState :: ML.Sigma s -> EventID -> UnfolderOp s (ML.Sigma s)
newState cst e = do
    s@UnfolderState{..} <- get
    let ev@Event{..} = getEvent e events
        t = ML.getTransition system etr
    res <- lift $ t cst
    case res of 
        Nothing -> error $ "newState: the transition was not enabled " ++ show cst
        Just fn -> lift $ fn cst

-- Check if two events are concurrent
-- Need to optimise this function: this is very inneficient!
-- The standard definition is: e || e' iff not (e < e' || e' < e || e # e')
-- This function is wrong!
isConcurrent :: EventID -> EventID -> State (UnfolderState s) Bool
isConcurrent e e' = do
  s@UnfolderState{..} <- get
  prede  <- predecessors e >>= return . (e:) 
  prede' <- predecessors e' >>= return . (e':)
  let cfle = fromMaybe [] $ M.lookup e immediateConflicts
  return $ not $ e' `elem` prede || e `elem` prede' || e' `elem` cfle

-- Useful Functions
predecessors, successors :: EventID -> Causality -> EventsID
{-# INLINABLE predecessors #-}
predecessors e causality = do
    s@UnfolderState{..} <- get
    let iprec = foldr (\(e1,e2) r -> if e2 == e then e1:r else r) [] causality
    iprec' <- mapM predecessors iprec
    return $ concat iprec' ++ iprec
{-# INLINABLE sucessors #-}
successors e causality = do
    s@UnfolderState{..} <- get
    let isucc = foldr (\(e1,e2) r -> if e1 == e then e2:r else r) [] causality
    isucc' <- mapM successors isucc
    return $ concat isucc'++ isucc

isMaximal :: Configuration s -> EventID -> State (UnfolderState s) Bool
isMaximal conf@Configuration{..} e = do
    es <- successors e
    return $ not $ any (\e' -> e' `elem` cevs) es 

isDependent :: ML.TransitionID -> EventID -> State (UnfolderState s) Bool
isDependent tr e = do
    s@UnfolderState{..} <- get
    let ev@Event{..} = getEvent e events
    return $ ML.isDependent indep tr etr

-- retrieves the event associated with the event id 
getEvent :: EventID -> Events -> Event
{-# INLINE getEvent #-}
getEvent e events =
  case M.lookup e events of
    Nothing -> error $ "getEvent: " ++ show e ++ ": with events=" ++ show events
    Just ev -> ev

getConfiguration :: ConfigurationID -> State (UnfolderState s) (Configuration s)
getConfiguration conf = do
    s@UnfolderState{..} <- get
    return configurations -- case M.lookup conf configurations of
        -- Nothing -> error $ "getConfiguration: " ++ show conf ++ ": with configurations=" ++ show configurations
        -- Just c -> return c
    
-- Since the enabled events of a configuration are pre-computed
-- this function just has to get from the map
enabled :: ConfigurationID -> State (UnfolderState s) EventsID
enabled conf = do 
    s@UnfolderState{..} <- get
    return $ fromMaybe [] $ M.lookup conf enable

addDisable :: EventID -> EventID -> State (UnfolderState s) ()
addDisable ê e = do  -- trace ("addDisable: " ++ show ê ++ " " ++ show e) $ do
    s@UnfolderState{..} <- get
    let dis = M.alter (addDisableAux e) ê disable
    put s{ disable = dis}

addDisableAux :: EventID -> Maybe EventsID -> Maybe EventsID
addDisableAux e Nothing = Just $ [e]
addDisableAux e (Just d) = Just $ e:d

freshCounter :: Either () () -> State (UnfolderState s) Counter
freshCounter choice = do
    s@UnfolderState{..} <- get
    let (ec,cc) = counters
    case choice of
        Left _ -> do
            let nec = ec+1
            put s{ counters = (nec,cc) }
            return ec
        Right _ -> do
            let ncc = cc+1
            put s{ counters = (ec,ncc) }
            return cc
            
