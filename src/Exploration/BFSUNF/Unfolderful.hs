module Unfolderful where

import Data.Map hiding (foldr, filter, map)
import qualified Data.Map as M
import Data.List (nub)
import qualified Debug.Trace as T
import Model
import APIStateful
import Printer
import System.IO.Unsafe

trace a b = b
--trace = T.trace

initprefix :: Sigma -> UnfoldingPrefix
initprefix s = 
  let pes = (M.fromList [(1,("bot",1))], [], [])
      confs = M.fromList [(1, ([1],s))]
  in (pes, confs)

unfolder :: (System, UIndependence) -> Unfolding
unfolder ((t,s),i) = fix (unfold ((t,s),i) (t,s)) (initprefix s)

fix :: (Eq b, Show b) => (b -> b) -> b -> b
fix f b = let b' = f b
          in trace ("fix with " ++ show b') $ 
             if b == b'
             then b
             else fix f b'

unfold :: (System, UIndependence) -> System -> UnfoldingPrefix -> UnfoldingPrefix
unfold (([],s),ind)     sys pes = pes
unfold (((t:ts),s),ind) sys pes = trace ("unfold: " ++ show (snd3 t)) $
  let hs = historyCandidates t ind pes
  in if M.null hs
     then unfold ((ts,s),ind) sys pes
     else 
       let pes' = M.foldWithKey (addEvent sys ind t) pes hs
       in if pes == pes'
          then unfold ((ts,s),ind) sys pes'
          else pes'     

maximalEvents :: System -> UnfoldingPrefix -> (Configurations, EventsID)
maximalEvents sys pes = 
    let cs = maximalConfigurations sys pes
    in (cs, concat $ fst $ unzip $ M.elems cs)
    
maximalConfigurations :: System -> UnfoldingPrefix -> Configurations
maximalConfigurations sys (_, confs) = M.filter (isMaximal sys) confs

isMaximal :: System -> (EventsID, Sigma) -> Bool
isMaximal ([],_) (_,s) = True
isMaximal (((_,_,t):ts),i) c@(_,s) = 
    case t s of 
        Nothing -> isMaximal (ts,i) c
        Just _  -> False
                
historyCandidates :: Transition -> UIndependence -> UnfoldingPrefix -> Configurations
historyCandidates t ind ((events,causality,_), confs) = trace "historyCandidates" $
     M.filterWithKey (isCandidate events ind t) confs

isCandidate :: Map EventID Event -> UIndependence -> Transition -> ConfigurationID -> (EventsID, Sigma) -> Bool
isCandidate events ind (pId, tId, t) cID (maxEvents, s) = 
    if isPresent tId cID events
    then False
    else case t s of
        Nothing -> False -- the transition is not enabled at this state
        Just s' -> all (dependentEvent events ind tId) maxEvents
        
isPresent :: TransitionID -> ConfigurationID -> Map EventID Event -> Bool
isPresent tID cID m = 
    let v = M.elems m
    in elem (tID,cID) v

dependentEvent :: Map EventID Event -> UIndependence -> TransitionID -> EventID -> Bool
dependentEvent events ind t1 e = 
    case M.lookup e events of
        Nothing -> error "dependentEvent"
        Just (t2,_) -> dependent ind t1 t2 -- instead of interfere

interfere ind t1 t2 = not $ elem (t1,t2) ind || elem (t2,t1) ind

linebreak = "-------------------------\n"
addEvent :: System -> UIndependence -> Transition -> ConfigurationID -> (EventsID, Sigma) -> UnfoldingPrefix -> UnfoldingPrefix
addEvent sys ind (_,tID,t) confID (maxEventsID, s) pes@((events,causality,conflict), confs) = 
  T.trace (linebreak ++ "current pes " ++ printUnfConf pes ++ "\nadding event with transition " ++ tID ++ " to configuration id: " ++ show confID ++ " with maximal elements " ++ show maxEventsID) $
    case t s of
      Nothing -> error ""
      Just s' -> 
        let nconfID = (M.size confs) + 1
            eID = (M.size events) + 1
            e = (tID, confID) -- nconfID -> e
            events' = M.insert eID e events --
            confs' = updateConfs sys ind events causality tID t eID maxEventsID confs -- $ M.insert nconfID ([eID], s') confs
            causality' = (map (\e' -> (e',eID)) maxEventsID) ++ causality
            conflicts = findConflicts ind confs' events causality eID e
            conflict' = (map (\e' -> (e',eID)) conflicts) ++ conflict 
            confs'' = combineConfs sys tID causality' conflict' t eID maxEventsID confs'
            pes' = ((events', causality', conflict'), confs'')
        in if isCutoff sys pes' eID 
           then T.trace "Found cutoff" $ pes
           else T.trace "Just added" $ pes' 

findConflicts :: UIndependence -> Configurations -> Events -> Causality -> EventID -> Event -> EventsID
findConflicts ind confs events causality eID (tID, confID) = trace "findConflicts" $
    case M.lookup confID confs of
        Nothing -> error ""
        Just (history,_) -> 
            let es = eID:(localConfiguration causality history)
                pes = trace ("localConf is:" ++ show es)$ M.filterWithKey (isConflict ind es tID) events
            in M.keys pes

localConfiguration :: Causality -> EventsID -> EventsID
localConfiguration causality es = trace ("localConfiguration: " ++ show es ++ " " ++ show causality) $
    nub $ es ++ concatMap (\e -> localConfiguration causality [e' | (e',x) <- causality, x==e]) es

isConflict :: UIndependence -> EventsID -> TransitionID -> EventID -> Event -> Bool
isConflict ind cand t eID (t',_) = not (elem eID cand) && interfere ind t t'

updateConfs :: System -> UIndependence -> Map EventID Event -> Causality -> TransitionID -> (Sigma -> Maybe Sigma) -> EventID -> EventsID -> Configurations -> Configurations
updateConfs sys ind events causality tID t eID maxEIDs confs = trace ("updateConfigurations with Transition: " ++ tID ++ "\n history: " ++ show maxEIDs ++ "\n") $ 
  snd $ M.foldWithKey (updateConf sys ind events causality tID t eID maxEIDs) (M.size confs, confs) confs

updateConf :: System -> UIndependence -> Map EventID Event -> Causality -> TransitionID -> (Sigma -> Maybe Sigma) -> EventID -> EventsID -> ConfigurationID -> (EventsID, Sigma) -> (Int, Configurations) -> (Int, Configurations)
updateConf sys ind events causality tID t eID maxEIDs cID (maxC, s) (counter,confs) = trace ("updateConfiguration: transition " ++ tID ++ "; configuration ID: " ++ show cID) $!
     if all (\e -> e `elem` maxC) maxEIDs
     then let cut = filter (\e -> not $ elem e maxEIDs) maxC
              cut' = localConfiguration causality cut
              trCut = map (getTransitionOfEvent events) cut'
          in if tID `elem` trCut || any (interfere ind tID) trCut
             then trace "updateConf Fail1: " $ (counter, confs)
             else 
              let maxC' = eID:cut
                  s' = case t s of
                     Nothing -> error $ "updateConf at event: " ++ show eID ++ " with transition: " ++ tID ++ " configuration ID: " ++ show cID ++ " state: " ++ show s
                     Just k -> k
                  counter' = counter+1
                  nconf = (maxC',s')
              in trace "updateConf Succ: " $ (counter', M.insert counter' nconf confs)
     else trace "updateConf Fail2: " $ (counter,confs) 

getTransitionOfEvent :: Map EventID Event -> EventID -> TransitionID
getTransitionOfEvent events e = 
    case M.lookup e events of
        Nothing -> error "getTransitionOfEvent"
        Just (tID,_) -> tID
    
combineConfs :: System -> TransitionID -> Causality -> Conflict -> (Sigma -> Maybe Sigma) -> EventID -> EventsID -> Configurations -> Configurations
combineConfs sys tID causality conflict t eID eHistory confs = trace ("combineConfigurations " ++ show confs ++ "\n" ++ show eID ++ "\n") $! 
  snd $ M.foldWithKey (combineConf sys tID causality conflict t eID eHistory) (M.size confs, confs) confs

combineConf :: System -> TransitionID -> Causality -> Conflict -> (Sigma -> Maybe Sigma) -> EventID -> EventsID -> ConfigurationID -> (EventsID, Sigma) -> (Int, Configurations) -> (Int, Configurations)
combineConf sys tID causality conflict t eID eHistory cID (maxC, s) (counter,confs) = T.trace ("combineConf at event: " ++ show eID ++ " with transition: " ++ tID ++ " configuration ID: " ++ show cID ++ " state: " ++ show s) $
    if all (\e -> e == 1 || check causality e maxC) eHistory 
    then let maxC' = filter(\e -> not $ e == 1 || e `elem` eHistory) maxC
         in if maxC' /= [] && all (isConcur causality conflict eID) maxC'
            then let eHistory' = eID:filter(\e -> not $ e == 1) maxC'
                     s' = case t s of
                         Nothing -> error $ "combineConf at event: " ++ show eID ++ " with transition: " ++ tID ++ " configuration ID: " ++ show cID ++ " state: " ++ show s
                         Just k -> k
                     counter' = counter+1
                     nconf = (eHistory',s')
                 in T.trace "combineConf Succ: " $ (counter', M.insert counter' nconf confs)
            else T.trace "combineConf Fail1: " $ (counter,confs) 
    else  T.trace "combineConf Fail2: " $ (counter,confs) 

successors :: Causality -> EventID -> EventsID
successors causality e1 = nub $ e1:concat [e2:successors causality e2 | (e,e2) <- causality, e == e1]

check :: Causality -> EventID -> EventsID -> Bool
check causality e maxC = 
    let succE = successors causality e
    in T.trace ("check with successors of " ++ show e ++ "=" ++ show succE ++ ", maximal elements are =" ++ show maxC) $
     any (\e' -> e' `elem` maxC) succE

isConcur ::Causality -> Conflict -> EventID -> EventID -> Bool
isConcur causality conflict  e1 e2 = -- 1 3
     let succe1 = localConfiguration causality [e1]
         succe2 = localConfiguration causality [e2]
         causalCondition = e1 `elem` succe2 || e2 `elem` succe1 
         conflictLeftCondition = any (\e -> (e1,e) `elem` conflict || (e,e1) `elem` conflict) succe2
         conflictRightCondition = any (\e -> (e2,e) `elem` conflict || (e,e2) `elem` conflict) succe1
         b = not $ causalCondition || conflictLeftCondition || conflictRightCondition
         --b = not $ e1 `elem` successors causality e2 || (e1,e2) `elem` conflict || (e2,e1) `elem` conflict
     in trace ("isConcur: " ++ show e1 ++ " " ++ show e2 ++ " is " ++ show b) $ b
