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
import Domain.Lattice
import Domain.MemAddr
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

{-
  The memory layout of POET is quite simple:
    The memory is a map from base memory addresses to values.
-}

-- | Concrete Memory Cell
type ConMCell = MemCell SymId () ConValue

instance Show ConMCell where
  show (MCell ty val) = show val

-- Concrete offsets
type ConOffs = Map ConValue ConValue
   
-- | Concrete Heap
type ConHeap = Map MemAddrBase ConOffs 

ppConHeap :: SymbolTable -> ConHeap -> String
ppConHeap symt m = ppConHeap' symt $ M.toList m

ppConHeap' :: SymbolTable -> [(MemAddrBase,ConOffs)] -> String
ppConHeap' symt [] = ""
ppConHeap' symt ((addr,offs):rest) = 
  let header = ppConOffs (ppMemAddrBase symt addr) offs
      body   = ppConHeap' symt rest
  in header ++ body

ppConOffs :: String -> ConOffs -> String
ppConOffs addr m = 
  if (take 9 addr == "@(pthread" || addr == "@(printf)")
  then ""
  else case M.toList m of
    [(ConVal (VInt 0),val)] -> addr ++ " = " ++ show val ++ "\n"
    _ ->  ppConOffs' addr $ M.toList m

ppConOffs' :: String -> [(ConValue,ConValue)] -> String
ppConOffs' addr [] = ""
ppConOffs' addr ((o,v):rest) = 
  let header = addr ++ "[" ++ show o ++ "] = " ++ show v ++ "\n"
      body   = ppConOffs' addr rest
  in header ++ body
    
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

instance PP ConState where
  pp symt (ConState heap ts _ _) = 
    let tstr = M.foldWithKey (\tid th rest -> ppThState symt tid th ++ rest) "" ts 
        hstr = "----------------HEAP----------------\n" ++ ppConHeap symt heap
    -- in tstr ++ hstr
    in hstr ++ tstr
    
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

ppThState :: SymbolTable -> TId -> ThState -> String
ppThState symt tid (ThState pos _ locals) = 
  let h = "----------------THREAD " ++ show tid++ "----------------\n"
      b = "Control Location: " ++ show pos ++ "\n"
      r = ppConHeap symt locals
  in h ++ b ++ r
 
-- | Initial state 
empty_cstate :: ConState
empty_cstate = ConState M.empty M.empty 1 False

instance Lattice ConState where
   bot         = ConState M.empty M.empty 0 True
   top         = error "top   for ConState"
   join  s1 s2 = error "join  for ConState"
   meet  s1 s2 = error "meet  for ConState"
   widen s1 s2 = error "widen for ConState"
   (?.)        = cs_bot
   (<=.)       = (==)

instance Projection ConState where
  controlPart st@ConState{..} = M.map th_pos cs_tstates
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
