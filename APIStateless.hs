{-#LANGUAGE RecordWildCards #-}
module APIStateless where

import Model
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace 

type EventsID = [EventID]
type EventID = Int

type Causality = [(EventID, EventID)]
type Conflict = [(EventID, EventID)]

data Event = Event {
    etr :: TransitionID
--  , econf :: ConfigurationID
} 
  deriving (Show,Eq,Ord)
  
type ConfigurationID = Int
data Configuration = Configuration {
    cevs :: EventsID
  , cst :: Sigma
}
  deriving Show

type Alternative = EventsID
type Alternatives = [Alternative]
type EventCounter = Int
type ConfigurationCounter = Int
type Counter = Int

data UnfolderState = 
    UnfolderState {
      system :: System                                       -- The system being analyzed
    , indep :: UIndependence                                 -- Independence relation
    , events :: M.Map EventID Event                          -- Visible events of the unfolding prefix
    , configurations :: Configuration -- M.Map ConfigurationID Configuration  -- ConfigurationID -> {Events that form the conf} !Can be removed?
    , causality :: Causality                            -- Causality
    , enable  :: M.Map ConfigurationID EventsID         -- Enabled events at a configuration
    , disable :: M.Map EventID EventsID                 -- D
    , alternatives :: M.Map EventID Alternatives        -- V
    , immediateConflicts :: M.Map EventID EventsID      -- #^
    , counters :: (EventCounter,ConfigurationCounter)
    }

initialState :: System -> UIndependence -> UnfolderState
initialState sys indep = 
    UnfolderState sys indep (M.singleton 0 $ Event "bot") (Configuration [] undefined) [] M.empty M.empty M.empty M.empty (1,0)
   -- UnfolderState sys indep (M.singleton 0 $ Event "bot") M.empty [] M.empty M.empty M.empty M.empty (1,0)

beg = "--------------------------------\n BEGIN Unfolder State          \n--------------------------------\n"
end = "\n--------------------------------\n END   Unfolder State          \n--------------------------------\n"
instance Show UnfolderState where
    show (u@UnfolderState{..}) = 
        beg ++ "UIndep: " ++ show indep ++ "\nEvents: " ++ show events ++ "\nCausality: " ++ show causality 
     ++ "\n" ++ show configurations ++ "\nEnabled: " ++ show enable 
     ++ "\nDisable: " ++ show disable ++ "\nAlternatives: " ++ show alternatives  ++ "\nImmConflicts: " ++ show immediateConflicts ++ "\nCounters: " 
     ++ show counters ++ end

gc :: UnfolderState -> UnfolderState
gc s@UnfolderState{..} = 
  let nevents = 0:(M.foldrWithKey (\e alts r -> nub $ e:(concat alts) ++ r) [] alternatives)
      events' = M.filterWithKey (\eID _ -> eID `elem` nevents) events
      causality' = filter (\(e1,e2) -> e1 `elem` nevents && e2 `elem` nevents) causality
      immediateCnfls = M.filterWithKey (\eID _ -> eID `elem` nevents) immediateConflicts
  in UnfolderState system indep events' configurations causality' enable disable alternatives immediateCnfls counters

newState :: Sigma -> EventID -> State UnfolderState Sigma
newState cst e = do
    s@UnfolderState{..} <- get
    ev@Event{..} <- getEvent e
    let t = getTransition system etr
    case t cst of 
        Nothing -> error $ "newState: the transition was not enabled " ++ show cst
        Just s  -> return s

isConcurrent :: EventID -> EventID -> State UnfolderState Bool
isConcurrent e e' = do
  s@UnfolderState{..} <- get
  prede  <- predecessors e >>= return . (e:) 
  prede' <- predecessors e' >>= return . (e':)
  let cfle = fromMaybe [] $ M.lookup e immediateConflicts
  return $ not $ e' `elem` prede || e `elem` prede' || e' `elem` cfle

-- Useful Functions
predecessors, successors :: EventID -> State UnfolderState EventsID
predecessors e = do
    s@UnfolderState{..} <- get
    let iprec = foldr (\(e1,e2) r -> if e2 == e then e1:r else r) [] causality
    iprec' <- mapM predecessors iprec
    return $ concat iprec' ++ iprec
successors e = do
    s@UnfolderState{..} <- get
    let isucc = foldr (\(e1,e2) r -> if e1 == e then e2:r else r) [] causality
    isucc' <- mapM successors isucc
    return $ concat isucc'++ isucc

isMaximal :: Configuration -> EventID -> State UnfolderState Bool
isMaximal conf@Configuration{..} e = do
    es <- successors e
    return $ not $ any (\e' -> e' `elem` cevs) es 

isDependent :: TransitionID -> EventID -> State UnfolderState Bool
isDependent tr e = do
    s@UnfolderState{..} <- get
    ev@Event{..} <- getEvent e
    return $ dependent indep tr etr

-- retrieves the event associated with the event id 
getEvent :: EventID -> State UnfolderState Event
getEvent e = do
    s@UnfolderState{..} <- get
    case M.lookup e events of
        Nothing -> error $ "getEvent: " ++ show e ++ ": with events=" ++ show events
        Just ev -> return ev

getConfiguration :: ConfigurationID -> State UnfolderState Configuration
getConfiguration conf = do
    s@UnfolderState{..} <- get
    return configurations -- case M.lookup conf configurations of
        -- Nothing -> error $ "getConfiguration: " ++ show conf ++ ": with configurations=" ++ show configurations
        -- Just c -> return c
    
-- Since the enabled events of a configuration are pre-computed
-- this function just has to get from the map
enabled :: ConfigurationID -> State UnfolderState EventsID
enabled conf = do 
    s@UnfolderState{..} <- get
    return $ fromMaybe [] $ M.lookup conf enable

addDisable :: EventID -> EventID -> State UnfolderState ()
addDisable ê e = do  -- trace ("addDisable: " ++ show ê ++ " " ++ show e) $ do
    s@UnfolderState{..} <- get
    let dis = M.alter (addDisableAux e) ê disable
    put s{ disable = dis}

addDisableAux :: EventID -> Maybe EventsID -> Maybe EventsID
addDisableAux e Nothing = Just $ [e]
addDisableAux e (Just d) = Just $ e:d

freshCounter :: Either () () -> State UnfolderState Counter
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
            
