{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Synchron
-- Copyright :  (c) 2015 Marcelo Sousa
-- 
-- Modules for the synchronisation semantics
-------------------------------------------------------------------------------
module Domain.Synchron where

import Haskroid.Hapiroid 
import Haskroid.Haskroid

type TId = Integer
type Pos = Integer

data System = 
  Sys
  { stid_ptr :: SteroidRef
  , poset    :: Poset
  , state    :: St 
  }

-- | A state is the sequence of positions in the partial order
type St = [Pos]

data PEvent = PEvent
  deriving (Show, Eq, Ord)
data Act = Act
  deriving (Show, Eq, Ord)

-- | Initial System
--   Initialize the steroid
--   Retrieve the first partial order 
stid_fe :: FilePath -> IO System
stid_fe file = do
  stid_ptr <- start_and_load file 
  poset    <- run_free stid_ptr
  let iSys = Sys stid_ptr poset []
  return iSys  

stid_end :: System -> IO ()
stid_end s@Sys{..} = terminate stid_ptr 

run :: System -> St -> (TId,Pos) -> St
run = undefined

-- initial "state"
fst_pos = undefined
enabled :: System -> St -> [PEvent]
enabled = undefined

pe_name = undefined
pe_act = undefined
pe_mut_addr = undefined
is_lock = undefined
is_unlock_of = undefined

psucc = undefined
