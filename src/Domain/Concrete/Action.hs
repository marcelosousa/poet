-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Action 
-- Copyright :  (c) 2016 Marcelo Sousa
-- Provides the implementation of the interface
-- to compute on-the-fly independece based on
-- read-write sets.
-------------------------------------------------------------------------------
module Domain.Concrete.Action where

import Domain.Concrete.State
import Model.GCS

-- Default implementation of an
-- Action using read write sets
-- over the memory addresses 
-- accessed.
data Act
  = Act
  { rds     :: MemAddrs -- read
  , wrs     :: MemAddrs -- write
  , locks   :: MemAddrs -- locks
  , unlocks :: MemAddrs -- unlocks
  }
  deriving (Eq,Ord)

instance Show Act where
  show act@Act{..} =
    let rs = "reads: " ++ show rds
        wrds = "writes: " ++ show wrs
        lks = "locks: " ++ show locks
        ulks = "unlocks: " ++ show unlocks
    in rs++"\n"++wrs++"\n"++lks++"\n"++ulks

{-
instance Action Act where
  varOf (Lock v) = v
  varOf (Unlock v) = v
  varOf Other = error "varOf Other"
  unlock = Unlock
  lock = Lock
  isActBlocking act =
    case act of
      Lock _ -> True
      Unlock _ -> True
      _ -> False 
  interferes acts others = case acts of
    [] -> False 
    (a:as) ->
      let ar = case a of
            Lock v -> any (\b -> b == Lock v || b == Unlock v) others
            Unlock v -> any (\b -> b == Lock v || b == Unlock v) others
            Read v -> any (\b -> b == Write v) others
            Write v -> any (\b -> b == Write v || b == Read v) others
            _ -> error "Other not supported"
      in ar || interferes as others
-}
