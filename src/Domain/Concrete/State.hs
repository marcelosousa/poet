{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.State
-- Copyright :  (c) 2015-17 Marcelo Sousa
--
-- The domain for the concrete semantics.
-------------------------------------------------------------------------------
module Domain.Concrete.State where

import Data.Hashable
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Domain.Concrete.Value
import Domain.MemAddr
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

{-
  The memory layout of POET is quite simple:
    The memory is a map from base memory addresses.
-}

-- | Concrete Memory Cell
type ConMCell = MemCell SymId () ConValue

instance Show ConMCell where
  show (MCell ty val) = show val

-- Concrete offsets
type ConOffs = Map ConValue ConValue

-- | Concrete Heap
type ConHeap = Map MemAddrBase ConOffs 

-- | Concrete state
data ConState = 
  ConState 
  { 
    cs_heap    :: ConHeap 
  , cs_tstates :: ThStates
  , cs_numth   :: Int
  , cs_bot     :: Bool 
  }
  deriving (Show,Eq,Ord)

-- | A thread state is a control and local data
type ThStates = Map TId ThState

-- | Thread state
data ThState =
  ThState
  { 
    th_pos    :: Pos
  , th_cfg_id :: SymId
  , th_locals :: ConHeap 
  } 
  deriving (Show,Eq,Ord)

-- Initial/Bottom values
bot_th_state :: Pos -> SymId -> ThState
bot_th_state pos id = ThState pos id M.empty

-- | Initial state 
bot_cstate :: ConState
bot_cstate = ConState M.empty M.empty 1 False

set_cstate_bot :: ConState -> ConState
set_cstate_bot i = i { cs_bot = True }

instance Projection ConState where
  controlPart st@ConState{..} = M.map th_pos cs_tstates
  subsumes a b = a == b
  isBottom = cs_bot 
  toThCFGSym st@ConState{..} tid = 
    case M.lookup tid cs_tstates of
      Nothing -> error $ "toThCFGSym: invalid tid " ++ show tid
      Just t@ThState{..} -> th_cfg_id 
  
instance Hashable ConState where
  hash s@ConState{..} = hash (cs_heap,cs_tstates,cs_numth,cs_bot) 
  hashWithSalt s st@ConState{..} = hashWithSalt s (cs_heap,cs_tstates,cs_numth,cs_bot)

instance Hashable ConHeap where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h
 
instance Hashable ConOffs where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h

instance Hashable ThStates where
  hash = hash . M.toList
  hashWithSalt s th = hashWithSalt s $ M.toList th

instance Hashable ThState where
  hash th@ThState{..} = hash (th_pos,th_cfg_id,th_locals)
  hashWithSalt s th@ThState{..} = hashWithSalt s (th_pos,th_cfg_id,th_locals)

instance Hashable ConMCell where
  hash m@MCell{..} = hash val
  hashWithSalt s m@MCell{..} = hashWithSalt s val
