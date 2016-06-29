{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Model.GCS where

import qualified Data.ByteString.Char8 as BS
import Data.Map hiding (foldr, filter, map, (\\), null)
import Language.SimpleC.AST 
import Language.SimpleC.Flow
import Language.SimpleC.Converter
import Util.Generic

-- A System is a collection of CFGs together with
-- a representation of the frontier on the PCs 
-- which denotes the number of enabled threads
-- and their position in the CFG. 
-- The CFGs are annotated with states at each node,
-- as in usual abstract interpretation where these
-- states are elements of a lattice.
-- These states not only contain valuations but
-- also information regarding the actions to
-- compute an independence relation on the fly.
-- AbsSystem denotes an abstract system that 
-- receives as type parameters: a representation 
-- for the state and a represetation for the actions.
data System st act = 
  System 
  { 
--    frnt :: Frontier                 -- ^ Frontier in the CFGs
    gbst :: st                       -- ^ Initial (Global) State 
  , gbac :: act                      -- ^ Initial (Global) Actions
  , cfgs :: Graphs SymId () (st,act) -- ^ Control Flow Graphs
  , symt :: Map SymId Symbol         -- ^ Symbol Table
  , thds :: [TId]                    -- ^ Threads ids
  , tcnt :: Int                      -- ^ Counter for th id
  }
  deriving Show

-- Frontier of in the CFGs
type Frontier = [NodeId]
-- Position in the CFG
type Pos = NodeId
-- Thread/Transition ID
type TId = Int

-- | botID -1 is the tid for bottom 
botID :: TId 
botID = -1

-- | The control part of a state 
--   is a map from thread id to position 
--   in the CFG.
type Control = Map TId Pos

class Projection st where
  controlPart :: st -> Control 
  subsumes :: st -> st -> Bool
  isBottom :: st -> Bool

class (Show act, Action act, Show st, Projection st) => Collapsible st act where
  enabled :: System st act -> st -> [TId]
  collapse :: System st act -> st -> TId -> [(st,Pos,act)]
  dcollapse :: System st act -> st -> (TId,Pos) -> (st,act)
  simple_run :: System st act -> st -> (TId,Pos) -> st
  simple_run sys st name = fst $ dcollapse sys st name
 
class (Eq act) => Action act where
  isBlocking :: act -> Bool
  -- Given two sets of actions a1 and a2,
  -- check if there exists in a2 an unlock 
  -- or lock with an address that is touched
  -- by a1.
  isUnlockOf :: act -> act -> Bool 
  isLockOf :: act -> act -> Bool 
  -- Two sets of actions are independent
  interferes :: act -> act -> Bool
