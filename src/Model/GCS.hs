{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}
{-#LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Model.GCS 
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- This module defines the model of computation and the main classes 
-- that need to be defined in order to add a new domain to the unfolder.
-------------------------------------------------------------------------------
module Model.GCS where

import Data.List
import Data.Map hiding (foldr, filter, map, (\\), null)
import Data.Set (Set)
import Domain.Lattice
import Language.SimpleC.AST 
import Language.SimpleC.Converter
import Language.SimpleC.Flow
import Language.SimpleC.Util hiding (cfgs)
import Util.Generic
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M 

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
-- for the state and a representation for the actions.
data System st act = 
  System 
  { 
--    frnt :: Frontier                 -- ^ Frontier in the CFGs
    gbst :: st                       -- ^ Initial (Global) State 
  , gbac :: act                      -- ^ Initial (Global) Actions
  , cfgs :: Graphs SymId () (st,act) -- ^ Control Flow Graphs
  , symt :: SymbolTable              -- ^ Symbol Table
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
type SymAddr = Int

-- | main_tid 0 is the tid for main
main_tid :: TId
main_tid = 0

-- | botID -1 is the tid for bottom 
botID :: TId 
botID = -1

-- | The control part of a state: 
--   map from thread id to CFG node
type Control = Map TId Pos

-- Projection defines the API over the "state" 
class Lattice st => Projection st where
  controlPart :: st -> Control
  toThCFGSym  :: st -> TId -> SymId

-- Properties of Values
class (Eq v, Ord v) => ToValue v where
   kVal :: Int -> v
   zero :: v
   zero = kVal 0
   one  :: v
   one  = kVal 1