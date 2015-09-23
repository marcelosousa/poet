{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.Cutoff.McMillan where

import Control.Monad.State.Strict
import Control.Monad.ST.Safe
import qualified Data.HashTable.Class as H
import Domain.Concrete
import Exploration.UNF.APIStateless

-- @ Check if there is a cutoff
cutoff :: Sigma s -> Int -> UnfolderOp s Bool
cutoff st si = do
    s@UnfolderState{..} <- get
    rst <- lift $ H.toList st
    mv <- lift $ H.lookup stas rst
    case mv of
      Nothing -> do
        lift $ H.insert stas rst si
        return False
      Just v -> return $ v < si

