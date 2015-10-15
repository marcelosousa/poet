{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}
module Model.GCS where

-- Data Structures
import qualified Data.ByteString as BS
import Data.List
import qualified Data.Map as M
import Data.Map hiding (foldr, filter, map, (\\), null)
import Data.Maybe 
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Word as W
import Debug.Trace
--import Domain.Concrete
import Language.SimpleC.AST (PC)
import Util.Generic

-- System is a vector of transitions and an initial state
-- We use vector because we just want to query. 
data System st = 
  System {
    transitions :: Transitions st,
    initialState :: st,
    initialActs :: Acts
  }

type ProcessID = BS.ByteString
type TransitionID = Int 
type TransitionsID = V.Vector TransitionID
type Transitions st = V.Vector (Transition st)
type TransitionInfo = (ProcessID, TransitionID, Acts)
type Transition st = (TransitionInfo, TransitionFn st)
type TransitionFn st = st -> [st]


data Variable = V Var | A Var Integer
  deriving (Show,Eq,Ord)
  
type Acts = [Act]
data Act = Lock Variable | Unlock Variable | Other
  deriving (Show,Eq,Ord)

varOf :: Act -> Variable
varOf (Lock v) = v
varOf (Unlock v) = v
varOf Other = error "varOf Other"

isBlocking :: Acts -> Bool
isBlocking = any (\act -> act /= Other)

class Projection st where
  controlPart :: st -> st
  dataPart :: st -> st
  subsumes :: st -> st -> Bool
  
-- | enabledTransitions
enabledTransitions :: System st -> st -> V.Vector TransitionInfo
enabledTransitions sys@System{..} s =
  let enTr = V.filter (\(_,t) -> not $ null $ t s) transitions
  in V.map fst enTr

-- | botID 0 is the transition id for bottom 
botID :: TransitionID
botID = -1

-- | bottom transition is simply: return . id
bot :: TransitionFn st
bot = (:[])

-- GETTERS

-- | getTransition - 
getTransition :: System st -> TransitionID -> TransitionFn st
getTransition sys@System{..} trIdx
  | trIdx == botID = bot
  | otherwise = 
      case transitions V.!? trIdx of
        Nothing -> error $ "getTransition fail: " ++ show trIdx
        Just (_,tr) -> tr

getTransitionWithID :: System st -> TransitionID -> TransitionFn st
getTransitionWithID sys@System{..} trID = 
  case transitions V.!? trID of
    Nothing -> error $ "getTransition fail: " ++ show trID
    Just ((_,trIDx,_),tr) -> 
      if trID /= trIDx
      then error $ "getTransitionWithID something went wrong before: " ++ show (trID, trIDx)
      else tr

