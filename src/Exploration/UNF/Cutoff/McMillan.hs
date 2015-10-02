{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.Cutoff.McMillan where

import Control.Monad.State.Strict
import Control.Monad.ST.Safe
import Data.Hashable
import qualified Data.HashTable.Class as H
import Domain.Concrete
import Exploration.UNF.APIStateless
--import Debug.Trace

-- @ Check if there is a cutoff
cutoff :: (Show st, Hashable st, Eq st) => st -> Int -> UnfolderOp st s Bool
cutoff st si = mtrace ("Checking cutoff with state: " ++ show st) $ do
  s@UnfolderState{..} <- get
  mv <- lift $ H.lookup stas st
  case mv of
    Nothing -> do
      lift $ H.insert stas st si
      return False
    Just v -> return True -- return $ v < si

