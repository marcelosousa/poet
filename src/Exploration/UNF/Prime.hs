module Exploration.UNF.Prime (primefactor) where

import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe hiding (catMaybes)
import qualified Data.Set as S
import Data.Set (isSubsetOf)

import qualified Model.GCS as GCS
import Exploration.UNF.APIStateless
import System.IO.Unsafe
import Prelude hiding (pred)
import Util.Generic
import qualified Debug.Trace as T

-- Return (Prefix, InitialSize, NewSize, NrPrime)
primefactor :: Events s -> ST s (Events s, Int, Int, Int)
primefactor evs = do 
  m <- H.toList evs
  -- EventIDs
  let initEvs = length m
      eIDs = fst $ unzip m
  -- For each EventID, compute the local configuration
  -- [(EventID, Predecessors)]
  eKPs' <- mapM (\e -> predecessors e evs >>= \es -> return (e,es)) eIDs
  -- Map EventID Event
  let eKPs = M.fromList eKPs'
      eKVs = M.fromList m
  -- Map TransitionID [(EventID, Event, Predecessors)] 
      primes = M.foldWithKey (\eID ev res -> prime eID ev eKPs res) M.empty eKVs
      pNum = length primes
      factors = M.fold (\l res -> concatMap (\x -> factorize x eKVs) l ++ res) [] primes
      result = nub factors
      fSize = length result
  prefix <- H.fromList result
  T.trace (show primes) $ return (prefix, initEvs, fSize, pNum)

factorize :: (EventID, Event, EventsID) -> Map EventID Event -> [(EventID, Event)]
factorize (eID, ev, preds) eIDev =
  let preds_ = map (\e -> case M.lookup e eIDev of 
                   Nothing -> error "cant find event"
                   Just k -> (e,k)) preds
  in (eID,ev):preds_
  
process :: (EventID, Event, EventsID) -> [(EventID, Event, EventsID)] -> [(EventID, Event, EventsID)]
process r@(eID,e,es) rest =
  let s = length es
      rest' = filter (\(a,b,c) -> length c < s) rest
  in if rest' == []
     then r:filter (\(a,b,c) -> length c == s) rest
     else if all (\(a,b,c) -> length c > s) rest
          then rest 
          else r:rest

prime :: EventID -> Event -> Map EventID (EventsID) -> Map GCS.TransitionID [(EventID, Event, EventsID)] -> Map GCS.TransitionID [(EventID, Event, EventsID)]
prime eID ev eKPs res = 
  let tr = snd3 $ evtr ev
  in case M.lookup eID eKPs of
    Nothing -> error "cant find predecessors of event"
    Just preds -> case M.lookup tr res of
      Nothing -> M.insert tr [(eID,ev,preds)] res
      Just ls -> M.insert tr (process (eID, ev, preds) ls) res