{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module Model where

import Control.Monad.ST.Safe
import Control.Monad
import Control.Monad.Trans.Maybe

-- Data Structures
import qualified Data.Map as M
import Data.Map hiding (foldr, filter, map, (\\))
import Data.List
import qualified Data.Judy as J
-- Perhaps I want to use Unboxed Vectors?
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import qualified Data.Word as W
import qualified Data.Set as S
import Data.Maybe 

import Debug.Trace

-- System is a vector of transitions and an initial state
-- We use vector because we just want to query. 
data System s = 
  System {
    transitions :: V.Vector (Transition s),
    initialState :: ISigma s,
    initialLState :: LSigma
  }

type ISigma s = Sigma s
-- A state is an Hash Table
type HashTable s k v = C.HashTable s k v
--  Later we can try Judy Arrays or Mutable Vectors
type Sigma s  = HashTable s Var Value -- We want to add the enabled transitions for efficiency.
type Var      = BS.ByteString
type Value    = Int

-- Local State
type LSigma = [(Var,Value)] 

--type Process = Map TransitionID Transition
type ProcessID = BS.ByteString
type TransitionID = Int 
type TransitionsID = V.Vector TransitionID
type Transition s = (ProcessID, TransitionID, TransitionFn s)
type TransitionFn s = Sigma s -> ST s (Maybe (Sigma s -> ST s (Sigma s,LSigma)))

-- | enabledTransitions 
enabledTransitions :: System s -> Sigma s -> ST s (V.Vector (TransitionID,ProcessID))
enabledTransitions sys@System{..} s = do
  s' <- copy s -- this is not necessary if the first part of the transition does not modify the state
  tr <- V.filterM (\(_,_,t) -> t s' >>= return . maybe False (const True)) transitions  
  V.mapM (\(a,b,c) -> return (b,a)) tr

-- An independence relation is an irreflexive and symmetric relation
-- Triangular Matrix to exploit symmetry of independence relation
-- The irreflexivity can be simply checked with equality which is O(1) 
type UIndep = V.Vector (V.Vector Bool)

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- END OF TYPES 

-- safeLookup: lookup :: (Eq k, Hashable k) => h s k v -> k -> ST s (Maybe v)
safeLookup :: String -> Sigma s -> Var -> ST s Value
safeLookup err ht k = do
  mv <- H.lookup ht k
  case mv of 
    Nothing -> error $ "safeLookup: " ++ err
    Just v  -> return v

-- It is many times the case that we run mutually exclusive functions
-- like what i have below (min, max). Program consolidation could 
-- optimisize this code.
-- | isIndependent -- check if two transitions are uncond. indep.
isIndependent, isDependent :: UIndep -> (TransitionID, ProcessID) -> (TransitionID, ProcessID) -> Bool
isIndependent uindep (t1,p1) (t2,p2)  
  | (t1 == botID) || (t2 == botID) || (t1 == t2) || (p1 == p2)  = False
  | otherwise = 
      let t  = min t1 t2
          t' = max t1 t2
      in uindep V.! t V.! t'
 
-- | isDependent - checks if two transitions are dependent
isDependent uindep t1 t2 = not $ isIndependent uindep t1 t2

-- | botID 0 is the transition id for bottom 
botID :: TransitionID
botID = -1

-- | bottom transition is simply: return . id
bot :: TransitionFn s
bot s = return $ Just (\s' -> return (s',[]))    

-- GETTERS

-- | getTransition - 
getTransition :: System s -> TransitionID -> TransitionFn s
getTransition sys@System{..} trIdx
  | trIdx == botID = bot
  | otherwise = 
      case transitions V.!? trIdx of
        Nothing -> error $ "getTransition fail: " ++ show trIdx
        Just (_,_,tr) -> tr


-- This needs to be more efficient
copy :: Sigma s -> ST s (Sigma s)
copy s = do 
  kv <- H.toList s
  H.fromList kv 

showSigma :: Sigma s -> ST s String
showSigma s = do
  kv <- H.toList s
  return $ show kv 

equals :: Sigma s -> Sigma s -> ST s Bool
equals s1 s2 = do
  kv1 <- H.toList s1
  kv2 <- H.toList s2
  return $ sort kv1 == sort kv2

-- Modifies the current state with some local states
modify :: Sigma s -> LSigma -> ST s (Sigma s)
modify s [] = return s
modify s ((k,v):r) = do 
  H.insert s k v
  modify s r
-- modify s l = trace ("modify: " ++ show l) $ H.fromList l
   
-- Add a state to a list of states if that state is not already in the list
add :: Sigma s -> [Sigma s] -> ST s [Sigma s]
add s sts = do
    return $ s:sts
--  isEl <- isElem s sts
--  if isEl
--  then return sts
--  else return $ s:sts

-- This is the bottleneck
isElem :: Sigma s -> [Sigma s] -> ST s Bool
isElem s [] = return False
isElem s (x:xs) = do
  isEq <- isEqual s x
  if isEq 
  then return True
  else isElem s xs

-- State equality: because I'm using a hashtable I need to stay within the ST monad
isEqual :: Sigma s -> Sigma s -> ST s Bool
isEqual s1 s2 = do
  l1 <- H.toList s1
  l2 <- H.toList s2
  return $ isEqual' l1 l2  

isEqual' :: [(Var,Value)] -> [(Var,Value)] -> Bool
isEqual' [] [] = True
isEqual' ((x,v):xs) ((y,t):ys) = x == y && v == t && isEqual' xs ys

{-
sortSigmas :: [Sigma s] -> [Sigma s]
sortSigmas sts = 
  let sts' = map filterSigma sts
  in sort sts'

filterSigma :: Sigma s -> Sigma s
filterSigma st = M.filter (==1) st
-}