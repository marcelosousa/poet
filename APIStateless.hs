{-#LANGUAGE RecordWildCards #-}
module APIStateless where

import Control.Monad.State.Strict
import Control.Monad.ST.Safe

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Model as ML

import Debug.Trace 

type EventsID = [EventID]
type EventID = Int

type Causality = [(EventID, EventID)]
type Conflict = [(EventID, EventID)]

data Event = Event {
    etr :: ML.TransitionID
} 
  deriving (Show,Eq,Ord)
  
type ConfigurationID = Int
data Configuration s = Conf {
    stc :: ML.Sigma s  -- state at this configuration
  , maxevs :: EventsID  -- maximal events of the configuration
  , enevs  :: EventsID  -- enabled events of the configuration
  , cevs   :: EventsID  -- special events: the ones that have imm conflicts
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
    -- , configurations :: Configuration s -- M.Map ConfigurationID Configuration  -- ConfigurationID -> {Events that form the conf} !Can be removed?
    , previousConf :: Configuration s
    , causality :: Causality                            -- Causality
    -- , enable  :: M.Map ConfigurationID EventsID         -- Enabled events at a configuration
    , disable :: M.Map EventID EventsID                 -- D
    , alternatives :: M.Map EventID Alternatives        -- V
    , immediateConflicts :: M.Map EventID EventsID      -- #^
    , counters :: (EventCounter,ConfigurationCounter)
    }

type UnfolderOp s a = StateT (UnfolderState s) (ST s) a

botEID :: EventID
botEID = 0

botEvent :: Event
botEvent = Event ML.botID

iUnfState :: ML.System s -> ML.UIndep -> UnfolderState s
iUnfState sys indep = 
    UnfolderState sys indep (M.singleton 0 $ botEvent) (Conf undefined [] [] []) [] M.empty M.empty M.empty (1,0)
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
-- TODO: FIX THIS FUNCTION
isConcurrent :: EventID -> EventID -> State (UnfolderState s) Bool
isConcurrent e e' = do
  s@UnfolderState{..} <- get
  let prede = e:predecessors e causality 
      prede' = e':predecessors e' causality
      -- imd conflicts of all prede cfle = fromMaybe [] $ M.lookup e immediateConflicts
      -- imd conflicts of all prede'
      -- check that e is not an imd clf of any prede' and vice versa
  return $ not $ e' `elem` prede || e `elem` prede' -- missing cnfl part

-- Useful Functions: Crazy expensive
-- TODO: FIX THIS FUNCTION
predecessors, successors :: EventID -> Causality -> EventsID
{-# INLINABLE predecessors #-}
predecessors e causality =
    let iprec = foldl' (\r (e1,e2) -> if e2 == e then e1:r else r) [] causality
        iprec' = concatMap (\e -> predecessors e causality) iprec
    in iprec ++ iprec'
{-# INLINABLE successors #-}
successors e causality = 
    let isucc = foldl' (\r (e1,e2) -> if e1 == e then e2:r else r) [] causality
        isucc' = concatMap (\e -> successors e causality) isucc
    in isucc ++ isucc'

-- TODO: FIX THIS FUNCTION
-- no need for this function because we represent a configuration as the frontier
--isMaximal :: Configuration s -> EventID -> Causality -> Bool
--isMaximal conf@Configuration{..} e causality = 
--    let es = successors e causality
--    in not $ any (\e' -> e' `elem` cevs) es 

-- This can be removed
isDependent :: ML.UIndep -> Events -> ML.TransitionID -> EventID -> Bool
{-# INLINE isDependent #-}
isDependent indep events tr e = 
    let ev@Event{..} = getEvent e events
    in ML.isDependent indep tr etr

-- path from e to e' in the causality
path :: Causality -> EventID -> EventID -> Bool
path = undefined

-- GETTERS
-- retrieves the event associated with the event id 
getEvent :: EventID -> Events -> Event
{-# INLINE getEvent #-}
getEvent e events =
  case M.lookup e events of
    Nothing -> error $ "getEvent: " ++ show e ++ ": with events=" ++ show events
    Just ev -> ev

allEventsOfConf :: Causality -> EventsID -> S.Set EventID
allEventsOfConf causa maxevs = 
  let ip = [i | (i,j) <- causa, any (==j) maxevs] 
  in S.union (S.fromList maxevs) $ allEventsOfConf causa ip
 
getImmediateConflicts :: EventID -> UnfolderOp s EventsID
getImmediateConflicts = undefined 

getDisabled :: EventID -> UnfolderOp s EventsID
getDisabled = undefined

--getConfiguration :: ConfigurationID -> State (UnfolderState s) (Configuration s)
--getConfiguration conf = do
--    s@UnfolderState{..} <- get
--    return configurations -- case M.lookup conf configurations of
        -- Nothing -> error $ "getConfiguration: " ++ show conf ++ ": with configurations=" ++ show configurations
        -- Just c -> return c
    
-- Since the enabled events of a configuration are pre-computed
-- this function just has to get from the map
-- enabled :: ConfigurationID -> State (UnfolderState s) EventsID
-- enabled conf = do 
--     s@UnfolderState{..} <- get
--     return $ fromMaybe [] $ M.lookup conf enable

addDisable :: EventID -> EventID -> State (UnfolderState s) ()
addDisable ê e = do  -- trace ("addDisable: " ++ show ê ++ " " ++ show e) $ do
    s@UnfolderState{..} <- get
    let dis = M.alter (addDisableAux e) ê disable
    put s{ disable = dis}

addDisableAux :: EventID -> Maybe EventsID -> Maybe EventsID
addDisableAux e Nothing = Just $ [e]
addDisableAux e (Just d) = Just $ e:d

freshCounter :: Either () () -> UnfolderOp s Counter
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
            
