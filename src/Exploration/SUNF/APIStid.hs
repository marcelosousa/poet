{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Exploration.SUNF.APIStid
-- Desc      :  API for communication between POET and STID 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Exploration.SUNF.APIStid where

import Control.Monad.State.Strict hiding (state)
import Domain.Synchron
import Exploration.SUNF.API
import Exploration.SUNF.State hiding (state)
import Haskroid.Hapiroid hiding (Poset, toPoset, Event)
import Haskroid.Haskroid
import qualified Data.Map as M
import qualified Debug.Trace as T

-- | Initial System
--   Initialize the steroid
--   Retrieve the first partial order 
stid_fe :: FilePath -> IO System
stid_fe file = do
  stid_ptr <- start_and_load file 
  poset_   <- run_free stid_ptr
  let poset = toPoset poset_
      iSys = Sys stid_ptr poset (initial_state poset) 
  putStrLn $ "stid_replay: " ++ show poset 
  return iSys  

stid_end :: System -> IO ()
stid_end s@Sys{..} = terminate stid_ptr 

stid_replay :: St -> EventsID -> UnfolderOp St
stid_replay st alte = do
  s@UnfolderState{..} <- get
  let stid = stid_ptr syst 
  (rep_, len_rep) <- build_replay alte  
  let rep = reverse rep_
  lift $ putStrLn $ "stid_replay: " ++ show (rep, len_rep) 
  po <- lift $ replay stid rep len_rep
  let poset = toPoset po 
      nsys  = Sys stid poset (state syst)
  lift $ putStrLn $ "stid_replay: " ++ show poset 
  set_sys nsys
  lift $ putStrLn $ "stid_replay:\n" ++ show_st st
  return $ run_replay nsys st (state syst)  
 
run_replay :: System -> St -> St -> St
run_replay sys s is = T.trace ("run_replay:\n" ++ show_st is) $
  let a = M.filterWithKey (\tid (p,_) -> 
       case M.lookup tid is of
         Nothing -> False
         Just (p',_) -> (p /= p')) s 
  in if M.null a
     then is
     else case enabled sys is of
         [] -> error "run_replay: not supposed to happen"
         l -> case filter (\pe -> case M.lookup (fromInteger $ tid pe) a of
                            Nothing -> False
                            Just _ -> True) l of
                [] -> error "run_replay: not supposed to happen anymore"
                (pe:_) -> let n = (tid pe, idx pe)
                          in run_replay sys s (run sys is n) 
 
build_replay :: EventsID -> UnfolderOp (Replay, Int)
build_replay alte = do
  lift $ print $ "build_replay: " ++ show alte
  s@UnfolderState{..} <- get
  evs <- lift $ mapM (\e -> get_event "build_replay" e evts) alte 
  return $ build_replay_es evs (0,0) ([],0)

build_replay_es :: [Event] -> (Int,Int) -> (Replay,Int) -> (Replay, Int)
build_replay_es [] (tid,c) (rep,n) =
  if c /= 0
  then (add_to_replay (tid,c) rep,n+1)
  else (rep,n)
build_replay_es (e@Event{..}:es) (tid,c) (rep,n) =
  let ntid = fromInteger $ fst name
  in if ntid == tid
     then build_replay_es es (tid,c+1) (rep,n)
     else build_replay_es es (ntid,1)  (add_to_replay (tid,c) rep, n+1) 

add_to_replay :: (Int,Int) -> Replay -> Replay
add_to_replay (th,nr) rep =
   (SteroidCTSW (fromInteger $ toInteger th) (fromInteger $ toInteger nr)):rep

