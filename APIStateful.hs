module APIStateful where

import Model
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

-- An Unfolding in a LPES
type UnfoldingPrefix = (PES, Configurations)
type Configurations = Map ConfigurationID Configuration 
type Unfolding = UnfoldingPrefix
type PES = (Events, Causality, Conflict)
type Events = Map EventID Event
type EventsID = [EventID]
type EventID = Int

type Causality = [(EventID, EventID)]
type Conflict = [(EventID, EventID)]

type Event = (TransitionID, ConfigurationID)
type ConfigurationID = Int
type Configuration = (EventsID, Sigma)

getConfiguration :: UnfoldingPrefix -> EventID -> Configuration
getConfiguration unf@((es,_,_),confs) e = 
  case M.lookup e es of
    Nothing -> error "getConfiguration: no event in the prefix"
    Just (_,cID) ->
      case M.lookup cID confs of
        Nothing -> error $ "getConfiguration: no configuration " ++ show cID ++ " in the prefix " ++ show unf
        Just c  -> c 

getState :: System -> UnfoldingPrefix -> EventID -> Sigma 
getState sys unf@((es,_,_),confs) e = 
  case M.lookup e es of
    Nothing -> error "getConfiguration: no event in the prefix"
    Just (tID,cID) -> 
      let t = getTransition sys tID
      in case M.lookup cID confs of
        Nothing -> error $ "getConfiguration: no configuration " ++ show cID ++ " in the prefix " ++ show unf
        Just (_,s) ->
          case t s of
           Nothing -> error "getState" 
           Just s' -> s' 

localConf :: UnfoldingPrefix -> EventID -> [EventID]
localConf unf@((es,causality,_),_) e = 
  let ipred = [x | (x,y) <- causality, y == e]
      rpred = concatMap (localConf unf) ipred
  in ipred ++ rpred 

isCutoff :: System -> UnfoldingPrefix -> EventID -> Bool
isCutoff sys unfpref e = isCutoffInLocal sys unfpref e

-- Implementing several cut-off criterias
-- The simplest one is to just look for cut-off events in the local configuration
isCutoffInLocal :: System -> UnfoldingPrefix -> EventID -> Bool
isCutoffInLocal sys unf e = 
  let es = localConf unf e
      ste = getState sys unf e
      stes = map (getState sys unf) es
  in trace ("ste:" ++ show ste ++ "\nstes: " ++ show stes) $ ste `elem` stes 

-- Esparza et el. criteria based on an total adequate order on configurations
-- Foata Normal Form of a Configuration
foata :: UnfoldingPrefix -> ConfigurationID -> a
foata = undefined


