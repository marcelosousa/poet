{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Exploration.UNF.State
-- Desc      :  Unfolder state used in the mature unfolder 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Exploration.UNF.State where

import Control.Monad.State.Strict
import Data.HashTable.IO 
import Data.List
import Data.Map (Map,fromList,empty)
import qualified Data.HashTable.IO as H
import qualified Model.GCS as GCS
import Language.SimpleC.AST
import Language.SimpleC.Flow

type Counter = Int

-- @ Various type definitions related to Events 
type EventID = Int
type EventsID = [EventID]
type EventName = (GCS.TId, GCS.Pos, SymId) -- SymId does not make sense here!
type EventInfo act = (EventName, act) 

-- @ Value of the main HashTable
--   (name, actions, predecessors, successors, #^, D, V)
data Event act =
  Event 
  {
    name :: EventName    -- ^ Event name 
  , acts :: act          -- ^ Event actions 
  , pred :: EventsID     -- ^ Immediate predecessors
  , succ :: EventsID     -- ^ Immediate successors
  , icnf :: EventsID     -- ^ Immediate conflicts: #^
  , disa :: EventsID     -- ^ Disabled events: D
  , alte :: Alternatives -- ^ Valid alternatives: V
  } 
  deriving (Eq,Ord)

-- @ Events represents the unfolding prefix as LPES
--   with a HashTable : EventID -> Event 
type Events act = CuckooHashTable EventID (Event act)

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
-- Int is the size of the local configuration of that event
-- type States st = HashTable s GCS.Control [(st,Int)]
type States st = Map GCS.Control [(st,Int)]

-- @ Options for the unfolder 
data UnfolderOpts =
  UnfOpts
  {
    stateless :: Bool 
  , cutoffs   :: Bool
  }
  deriving (Show,Eq,Ord)

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

-- @ The state of the unfolder at any moment
data UnfolderState st act = 
  UnfolderState
  {
    syst :: GCS.System st act -- ^ The system being analyzed
  , evts :: Events act        -- ^ Unfolding prefix 
  , pcnf :: Configuration st  -- ^ Previous configuration
  , stak :: EventsID          -- ^ Call stack
  , cntr :: Counter           -- ^ Event counter
  , stas :: States st         -- ^ Hash Table for cutoffs
  , opts :: UnfolderOpts      -- ^ Options
  , stats :: UnfolderStats    -- ^ Statistics 
  , widen :: Map NodeId Int   -- ^ Widening counters per CFG node
  , ewide :: Map EventName Int -- ^ Widening counters per EventName
}

-- @ Abbreviation of the type of an operation of the unfolder
type UnfolderOp st act a = StateT (UnfolderState st act) IO a

--------------------------------------------------------------
-- | Show and Print Printing
instance Show (UnfolderState st act) where
    show (u@UnfolderState{..}) = show stats 

instance Show act => Show (Event act) where
  show (Event name acts pred succ icnf disa alte) = 
    "E " ++ show name 
    -- ++ ", acts = " ++ show acts 
    ++ ", pred = " ++ show pred
    ++ ", succ = " ++ show succ ++ ", icnf = " ++ show icnf 
    ++ ", disa = " ++ show disa ++ ", alte = " ++ show alte 
 
-- @ Show the set of events
showEvents :: Show act => Events act -> IO String
showEvents evs = do
  m <- H.toList evs
  let km = sortBy (\a b -> compare (fst a) (fst b)) m
      r = foldl (\a m -> show m ++ "\n" ++ a) "" km
  return r

print_stats :: Counter -> UnfolderStats -> IO ()
print_stats cnt u@UnfStats{..} = do
  print $ "nr of maximal configurations: " ++ show nr_max_conf
  print $ "nr of events: " ++ show cnt 
  print $ "size of prefix: " ++ show nr_evs_prefix 
