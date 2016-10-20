{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Exploration.SUNF.APIStid
-- Desc      :  API for communication between POET and STID 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Exploration.SUNF.APIStid where

import Control.Monad.State.Strict
import Domain.Synchron
import Exploration.SUNF.API
import Exploration.SUNF.State
import Haskroid.Hapiroid hiding (Poset, toPoset)

-- | Initial System
--   Initialize the steroid
--   Retrieve the first partial order 
stid_fe :: FilePath -> IO System
stid_fe file = do
  stid_ptr <- start_and_load file 
  poset_   <- run_free stid_ptr
  let poset = toPoset poset_
      iSys = Sys stid_ptr poset (initial_state poset) 
  return iSys  

stid_end :: System -> IO ()
stid_end s@Sys{..} = terminate stid_ptr 

stid_replay :: EventsID -> EventsID -> UnfolderOp ()
stid_replay alte stak = do
  s@UnfolderState{..} <- get
  let stid = stid_ptr syst 
  (rep, len_rep) <- build_replay alte  
  po <- lift $ replay stid rep len_rep
  -- need to put itself in the right position
  let pos   = move_to po stak
      poset = toPoset po 
      nsys  = Sys stid poset pos
  set_sys nsys
  
build_replay :: EventsID -> UnfolderOp (Replay, Int)
build_replay alte = undefined

move_to = undefined 
