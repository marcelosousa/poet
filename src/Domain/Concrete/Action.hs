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
type Acts = [Act]
data Act =
    Lock Variable 
  | Unlock Variable 
  | Write Variable
  | Read Variable
  | Other -- not currently used
  deriving (Eq,Ord)

instance Show Act where
  show act = case act of
    Other              -> "Other"
    Lock (V var)       -> "Lock " ++ show var
    Lock (A var idx)   -> "Lock " ++ show var ++ " " ++ show idx
    Unlock (V var)     -> "Unlock" ++ show var
    Unlock (A var idx) -> "Unlock" ++ show var ++ " " ++ show idx

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
