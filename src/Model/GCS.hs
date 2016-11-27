{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}
{-#LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Model.GCS 
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- General Computation System Model
-- General collapse 
--  For simplicity, a thread is enabled if the control
--  state is non-negative.
-------------------------------------------------------------------------------
module Model.GCS where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M 
import Data.Map hiding (foldr, filter, map, (\\), null)
import Language.SimpleC.AST 
import Language.SimpleC.Flow
import Language.SimpleC.Converter
import Language.SimpleC.Util hiding (cfgs)
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

-- | main_tid 0 is the tid for main
main_tid :: TId
main_tid = 0

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
  toThSym :: st -> TId -> SymId
  toThCFGSym :: st -> TId -> SymId

class (Show act, Action act, Show st, Projection st) => Collapsible st act where
  enabled :: System st act -> st -> [TId]
  enabled syst st =
    let control = controlPart st
        en = M.filterWithKey (\tid pos ->
          let tid_cfg_sym = toThCFGSym st tid
          in case M.lookup tid_cfg_sym (cfgs syst) of 
            Nothing  -> error $ "enabled fatal: tid " ++ show tid ++ " not found in cfgs"
            Just cfg -> not $ null $ succs cfg pos) control
    in M.keys en
  collapse :: Bool -> System st act -> st -> TId -> [(st,Pos,act)]
  dcollapse :: System st act -> st -> (TId,Pos,SymId) -> (st,act)
  dcollapse syst st (tid,pos,_) =
    let results = collapse False syst st tid
        result = filter (\(s,p,a) -> p == pos) results
    in case result of
      [] -> error "dcollapse: collapse does not produce dataflow fact at desired location"
      [(st,_,act)] -> (st,act)
      _ -> error "dcollapse: collapse produced several dataflow facts for desired location"
  simple_run :: System st act -> st -> (TId,Pos,SymId) -> st
  simple_run sys st name = fst $ dcollapse sys st name
 
class (Eq act) => Action act where
  isBlocking :: act -> Bool
  -- Given two sets of actions a1 and a2,
  -- check if there exists in a2 an unlock 
  -- or lock with an address that is touched
  -- by a1.
  isUnlockOf :: act -> act -> Bool 
  isLockOf :: act -> act -> Bool
  isCreateOf :: SymId -> act -> Bool 
  -- Two sets of actions are independent
  interferes :: act -> act -> Bool
  isGlobal :: act -> Bool
