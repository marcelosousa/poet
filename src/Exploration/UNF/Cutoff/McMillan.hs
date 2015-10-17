{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.Cutoff.McMillan where

import Control.Monad.State.Strict
import Control.Monad.ST.Safe
import Data.Hashable
import qualified Data.HashTable.Class as H
import qualified Data.Map as M
import Exploration.UNF.APIStateless
import qualified Model.GCS as GCS

--import Debug.Trace

-- @ Check if st is a cutoff
cutoff :: (Show st, Hashable st, Ord st, GCS.Projection st) => st -> Int -> UnfolderOp st s Bool
cutoff st si = trace ("Checking cutoff with state: " ++ show st) $ do
  s@UnfolderState{..} <- get
  let locs = GCS.controlPart st
  case M.lookup locs stas of
    Nothing -> do
      let stas' = M.insertWith (++) locs [(st,si)] stas
      updateCutoffTable stas'
      return False
    Just l -> do 
      c <- cutoffCheck st si l
      if c
      then return True
      else do
        let stas' = M.insertWith (++) locs [(st,si)] stas
        updateCutoffTable stas'
        return False          

cutoffCheck :: (Show st, Hashable st, Ord st, GCS.Projection st) => st -> Int -> [(st,Int)] -> UnfolderOp st s Bool
cutoffCheck st si [] = return False
cutoffCheck st si ((st',si'):r) = trace ("checking against: " ++ show st') $ do
  if st' `GCS.subsumes` st -- && si' < si
  then mtrace ("Cutoff is: " ++ show st') $ return True
  else cutoffCheck st si r
{-
  mv <- lift $ H.lookup stas st
  case mv of
    Nothing -> do
      lift $ H.insert stas st si
      return False
    Just v -> return True -- return $ v < si
-}
