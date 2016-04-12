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
      pNum = M.size primes
      -- Initial Prefix is the prefix of the sure primes
      (primesSure, primesUnsure) = M.partition (\l -> length l == 1) primes
      pNumSure = M.size primesSure
      -- Compute the complete prefix based on the initial prefix
      -- Order the remaining events
      pU = M.elems primesUnsure
      finalPrimes = decideUnsure pU $ concat $ M.elems primesSure
      -- 
      _prefix = foldr (\l res -> factorize l eKVs ++ res) [] finalPrimes
      prefix = nub _prefix      
      fSize = length prefix
  prefix <- H.fromList prefix
  --T.trace (printPrimes primes) $ return (prefix, initEvs, fSize, pNum)
  return (prefix, initEvs, fSize, pNumSure)
  
printPrimes :: Map GCS.TransitionID [(EventID, Event, EventsID)] -> String
printPrimes = M.foldWithKey (\k v s -> printPrime k v ++ s) ""  
printPrime :: GCS.TransitionID -> [(EventID, Event, EventsID)] -> String
printPrime t p = show t ++ ": " ++ foldr (\(eID, ev, preds) s -> show (eID, evtr ev) ++ " preds = " ++ show preds ++ "\n" ++ s) "" p

decideUnsure :: [[(EventID, Event, EventsID)]] -> [(EventID, Event, EventsID)] -> [(EventID, Event, EventsID)]
decideUnsure [] primes = primes
decideUnsure unsure primes =
  let pU' = sort $ map sort unsure
      p = decideU (head pU') primes
  in decideUnsure (tail pU') (p:primes)

isElem :: EventID -> [(EventID, Event, EventsID)] -> Bool
isElem eID [] = False
isElem eID ((k,_,_):r) 
  | eID == k = True
  | otherwise = isElem eID r

decideU :: [(EventID, Event, EventsID)] -> [(EventID, Event, EventsID)] -> (EventID, Event, EventsID)
decideU l primes =
  let npr = sort $ map (\k@(eID,_,preds) -> (foldl (\acc x -> if isElem x primes then acc + 1 else acc) 0 preds, eID,k)) l 
      (m,_,_) = maximum npr
      mpr = filter (\(a,b,c) -> a == m) npr
      mpr' = sortBy (\(a,b,c) (a',b',c') -> compare b b') mpr
      (_,_,e) = head mpr'
  in e
  
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
     else rest'

prime :: EventID -> Event -> Map EventID (EventsID) -> Map GCS.TransitionID [(EventID, Event, EventsID)] -> Map GCS.TransitionID [(EventID, Event, EventsID)]
prime eID ev eKPs res = 
  let tr = snd4 $ evtr ev
  in case M.lookup eID eKPs of
    Nothing -> error "cant find predecessors of event"
    Just preds -> case M.lookup tr res of
      Nothing -> M.insert tr [(eID,ev,preds)] res
      Just ls -> M.insert tr (process (eID, ev, preds) ls) res