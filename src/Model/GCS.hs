{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Model.GCS where

-- Data Structures
import qualified Data.ByteString as BS
import Data.List
import qualified Data.Map as M
import Data.Map hiding (foldr, filter, map, (\\), null)
import Data.Maybe 
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Word as W
import Debug.Trace
--import Domain.Concrete
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
type Frontier = [NodeId]
data System st act = 
  System 
  { frnt :: Frontier                 -- ^ Frontier in the CFGs
  , gbst :: st                       -- ^ Global State
  , gbac :: [act]                    -- ^ Actions 
  , cfgs :: Graphs SymId () (st,act) -- ^ Control Flow Graphs
  , symt :: Map SymId Symbol         -- ^ Symbol Table
  , thds :: [ThreadId]               -- ^ Thread ids
  , tcnt :: Int                      -- ^ Counter for th id
  }
  deriving Show

-- Thread ID
type ThreadId = Int

-- | botID -1 is the transition id for bottom 
botID :: ThreadId 
botID = -1

class Action act where
  varOf :: act -> Variable
  isBlocking :: [act] -> Bool

class Projection st where
  controlPart :: st -> st
  dataPart :: st -> st
  subsumes :: st -> st -> Bool

class (Action act, Projection st) => Collapsible st act where
  enabledThreads :: System st act -> st -> [ThreadId]
  collapse :: System st act -> st -> ThreadId -> [(st,[act])]

-- Default implementation of an
-- Action.
data Variable = V Var | A Var Integer
  deriving (Show,Eq,Ord)
  
type Acts = [Act]
data Act = Lock Variable | Unlock Variable | Other
  deriving (Show,Eq,Ord)

instance Action Act where
  -- varOf :: Act -> Variable
  varOf (Lock v) = v
  varOf (Unlock v) = v
  varOf Other = error "varOf Other"
  -- isBlocking :: Acts -> Bool
  isBlocking = any (\act -> act /= Other)
