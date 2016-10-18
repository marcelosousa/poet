{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Exploration.SUNF.State
-- Desc      :  Unfolder state used in this unfolder 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Exploration.SUNF.State where

import Prelude hiding (succ)

import Control.Monad.State.Strict
import Control.Monad.ST
-- Data Structures
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.Map (Map,fromList,empty)
import qualified Data.Map as MA
import qualified Data.Maybe as M
import Data.List
import Util.Generic

import Domain.Synchro
import qualified Debug.Trace as T

mytrace True a b = T.trace a b
mytrace False a b = b

-- @ The most basic type is event_id :: Int
--   Pointer to an event
type EventID   = Int
type EventsID  = [EventID]
type EventName = (TId, Pos)
type EventInfo = (EventName, Act) 

-- @ Value of the main HashTable
--   (name, actions, predecessors, successors, #^, D, V)
data Event =
  Event 
  {
    name :: EventName    -- ^ Event name 
  , acts :: Act          -- ^ Event actions 
  , pred :: EventsID     -- ^ Immediate predecessors
  , succ :: EventsID     -- ^ Immediate successors
  , icnf :: EventsID     -- ^ Immediate conflicts: #^
  , disa :: EventsID     -- ^ Disabled events: D
  , alte :: Alternatives -- ^ Valid alternatives: V
  } 
  deriving (Show,Eq,Ord)

-- @ Events represents the unfolding prefix as LPES
--   with a HashTable : EventID -> Event 
type Events s = HashTable s EventID Event 

-- @ Show the set of events
showEvents :: Events s -> ST s String
showEvents evs = do
  m <- H.toList evs
  let km = sortBy (\a b -> compare (fst a) (fst b)) m
      r = foldl (\a m -> show m ++ "\n" ++ a) "" km
  return r

-- @ Counter for various purposes
type Counter = Int

-- @ Configuration  
data Configuration =
  Conf 
  {
    maevs :: EventsID  -- ^ maximal events 
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

print_stats :: Counter -> UnfolderStats -> IO ()
print_stats cnt u@UnfStats{..} = do
  print $ "nr of maximal configurations: " ++ show nr_max_conf
  print $ "nr of events: " ++ show cnt 
  print $ "size of prefix: " ++ show nr_evs_prefix 

default_unf_stats :: UnfolderStats
default_unf_stats =
 let nr_max_conf = 0
     nr_cutoffs = 0
     nr_evs_prefix = 1
     sum_size_max_conf = 0
     nr_evs_per_name = MA.empty 
 in UnfStats nr_max_conf nr_cutoffs nr_evs_prefix
             sum_size_max_conf nr_evs_per_name

-- @ The state of the unfolder at any moment
data UnfolderState s = 
  UnfolderState
  {
    syst  :: System            -- ^ The system being analyzed
  , evts  :: Events s          -- ^ Unfolding prefix 
  , pcnf  :: Configuration     -- ^ Previous configuration
  , stak  :: EventsID          -- ^ Call stack
  , cntr  :: Counter           -- ^ Event counter
  , opts  :: UnfolderOpts      -- ^ Options
  , stats :: UnfolderStats     -- ^ Statistics 
  }

-- @ Abbreviation of the type of an operation of the unfolder
type UnfolderOp s a = StateT (UnfolderState s) (ST s) a

-- @ Initial state of the unfolder
i_unf_state:: Bool -> Bool -> System -> ST s (UnfolderState s) 
i_unf_state stateless_mode cutoff_mode syst = do
  evts <- H.new
  let pcnf = Conf [] [] [] 
      stak = []
      cntr = 0
      opts = UnfOpts stateless_mode cutoff_mode
  return $ UnfolderState syst evts pcnf stak cntr opts default_unf_stats 

instance Show (UnfolderState s) where
    show (u@UnfolderState{..}) = show stats 
