{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}
module Model.GCS where

import Control.Monad.ST.Safe
import Control.Monad
import Control.Monad.Trans.Maybe

-- Data Structures
import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Data.List
import qualified Data.Map as M
import Data.Map hiding (foldr, filter, map, (\\))
import Data.Maybe 
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Word as W
import Debug.Trace
import Domain.Concrete
import Language.SimpleC.AST (PC)

-- System is a vector of transitions and an initial state
-- We use vector because we just want to query. 
data System s = 
  System {
    transitions :: V.Vector (Transition s),
    initialState :: ISigma s,
    initialActs :: Acts
  }

--type Process = Map TransitionID Transition
type ProcessID = BS.ByteString
type TransitionID = Int 
type TransitionsID = V.Vector TransitionID
type TransitionMeta = (ProcessID, TransitionID, Acts)
type Transition s = (ProcessID, TransitionID, Acts, TransitionFn s)
type TransitionFn s = Sigma s -> ST s (Maybe (Sigma s -> ST s (Sigma s)))

-- read write data type
data RW = Read Variable | Write Variable
  deriving (Show,Eq,Ord)

type RWSet = [RW]

data Variable = V Var | A Var Integer
  deriving (Show,Eq,Ord)
  
type Acts = [Act]
data Act = Lock Variable | Unlock Variable | Other
  deriving (Show,Eq,Ord)

varOf :: Act -> Variable
varOf (Lock v) = v
varOf (Unlock v) = v
varOf Other = error "varOf Other"
-- 1. Variable or Array, Constant Index
--    
-- 2. Global state reached by the local configuration

isBlocking :: Acts -> Bool
isBlocking = any (\act -> act /= Other)

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


-- END OF TYPES 


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
bot s = return $ Just (\s' -> return s')    

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

