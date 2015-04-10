{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}
module Model.GCS where

import Control.Monad.ST.Safe
import Control.Monad
import Control.Monad.Trans.Maybe

-- Data Structures
import qualified Data.Map as M
import Data.Map hiding (foldr, filter, map, (\\))
import Data.List
-- import qualified Data.Judy as J
-- Perhaps I want to use Unboxed Vectors?
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import qualified Data.Word as W
import qualified Data.Set as S
import Data.Maybe 

import Debug.Trace
import Language.SimpleC.AST (PC)

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
type Sigma s = HashTable s Var Value -- We may want to add the enabled transitions for efficiency.
type Var = BS.ByteString
type SigmaValue = (Value, Maybe LockedValue)
data Value = 
      IntVal Int 
    | Array [Value]
  deriving (Show,Eq,Ord)

data LockedValue = 
      Var [(Var,PC)]
    | ArrayLock [LockedValue]
  deriving (Show,Eq,Ord)
  
-- Local State
type LSigma = [(Var,Value)]

--type Process = Map TransitionID Transition
type ProcessID = BS.ByteString
type TransitionID = Int 
type TransitionsID = V.Vector TransitionID
type TransitionMeta = (ProcessID, TransitionID, Act)
type Transition s = (ProcessID, TransitionID, Act, TransitionFn s)
type TransitionFn s = Sigma s -> ST s (Maybe (Sigma s -> ST s (Sigma s,LSigma)))

data Act = Lock Var | Unlock Var | Other
  deriving (Show,Eq,Ord)
-- 1. Variable or Array, Constant Index
--    
-- 2. Global state reached by the local configuration
showTransition :: Transition s -> String
showTransition (a,b,c,_) = show (a,b,c)

-- | enabledTransitions 
enabledTransitions :: System s -> Sigma s -> ST s (V.Vector TransitionMeta)
enabledTransitions sys@System{..} s = do
  s' <- copy s -- this is not necessary if the first part of the transition does not modify the state
  tr <- V.filterM (\(_,_,_,t) -> t s' >>= return . maybe False (const True)) transitions  
  V.mapM (\(a,b,c,d) -> return (a,b,c)) tr

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
isIndependent, isDependent :: UIndep -> TransitionMeta -> TransitionMeta -> Bool
isIndependent uindep (p1,t1,_) (p2,t2,_)  
  | (t1 == botID) || (t2 == botID) || (t1 == t2) || (p1 == p2)  = False
  | otherwise = 
      let t  = min t1 t2
          t' = max t1 t2
      in uindep V.! t V.! t'
 
-- | isDependent - checks if two transitions are dependent
isDependent uindep t1 t2 = not $ isIndependent uindep t1 t2

getIndepTr :: UIndep -> [TransitionMeta] -> [(TransitionMeta,TransitionMeta)]
getIndepTr uindep trs = [ (t1,t2) | t1 <- trs, t2 <- trs, t1 < t2 && isIndependent uindep t1 t2]

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
        Just (_,_,_,tr) -> tr

getTransitionWithID :: System s -> TransitionID -> TransitionFn s
getTransitionWithID sys@System{..} trID = 
  case transitions V.!? trID of
    Nothing -> error $ "getTransition fail: " ++ show trID
    Just (_,trIDx,_,tr) -> 
      if trID /= trIDx
      then error $ "getTransitionWithID something went wrong before: " ++ show (trID, trIDx)
      else tr 

-- This needs to be more efficient
copy :: Sigma s -> ST s (Sigma s)
copy s = do 
  kv <- H.toList s
  H.fromList kv 

showSigma :: Sigma s -> ST s String
showSigma s = do
  kv <- H.toList s
  return $ showSigma' kv 

showSigma' :: [(Var, Value)] -> String
showSigma' [] = ""
showSigma' ((v,vs):rest) = show v ++ "=" ++ show vs ++ "\n" ++ showSigma' rest
      
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
