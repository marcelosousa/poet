{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.Cutoff.McMillan where

import Control.Monad.State.Strict
import qualified Data.Map as M
import Exploration.UNF.APIStateless
import qualified Model.GCS as GCS

-- @ Check if st is a cutoff
cutoff :: GCS.Projection st => st -> Int -> UnfolderOp st act s Bool
cutoff st si = do
  s@UnfolderState{..} <- get
  let locs = GCS.controlPart st
      stas' = M.insertWith (++) locs [(st,si)] stas
  set_cutoff_table stas'
  case M.lookup locs stas of
    Nothing -> return False
    Just l -> return $ cutoffCheck st si l

cutoffCheck :: GCS.Projection st => st -> Int -> [(st,Int)] -> Bool
cutoffCheck st si [] = False
cutoffCheck st si ((st',si'):r) =
  (st' `GCS.subsumes` st && si' < si) || (cutoffCheck st si r)
{-
  mv <- lift $ H.lookup stas st
  case mv of
    Nothing -> do
      lift $ H.insert stas st si
      return False
    Just v -> return True -- return $ v < si
-}
