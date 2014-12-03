{-#LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Exploration.ODPOR where

import qualified Model.Language as L
import Model.Language (System, Transition, Var, Execution, enabled, Tag(..), Events, Event, Races, Race, PID)
import Exploration.WakeupTree
import Exploration.Auxiliary

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import qualified Data.Vector as V

-- Explore function
-- Algorithm 2
type SleepSet = [Var]
    
putWuTs :: WakeupTrees -> State ExploreState ()
putWuTs wuts' = do
    s@ExploreState{..} <- get
    put s{wuts=wuts'}

putSleeps :: SleepSets -> State ExploreState ()
putSleeps sleeps' = do
    s@ExploreState{..} <- get
    put s{sleeps=sleeps'}

putExecution :: Execution -> State ExploreState ()
putExecution eseq' = do
    s@ExploreState{..} <- get
    put s{eseq=eseq'}

type WakeupTrees = M.Map Execution WakeupTree  
type SleepSets   = M.Map Execution SleepSet
type StackStates = M.Map Execution L.State

data ExploreState = ExploreState 
    { sys      :: System
    , eseq     :: Execution
    , sleep    :: SleepSet
    , wut      :: WakeupTree
    , sleeps   :: SleepSets
    , wuts     :: WakeupTrees
    , curstate :: L.State
    , states   :: StackStates
    }
 
explore :: State ExploreState ()
explore = do
    s@ExploreState{..} <- get
    case enabled curstate of                         -- Line 3
        Nothing -> do
            races <- findRaces
            forM_ races raceAnalysis
        Just p  ->
          if not(isEmpty(wut))                       -- Line 10
          then do 
              putWuTs $ M.insert eseq wut wuts       -- Line 11
              putSleeps $ M.insert eseq sleep sleeps -- Line 15
              exploreNext
          else do
              let wut' = [[p]]
              putWuTs $ M.insert eseq wut' wuts      -- Line 14
              putSleeps $ M.insert eseq sleep sleeps -- Line 15
              exploreNext

raceAnalysis :: Race -> State ExploreState ()
raceAnalysis (e,e') = do
    s@ExploreState{..} <- get
    let eseq' = pre(eseq, e)                         -- Line 5
        v = notdep(e, eseq) ++ [e']                  -- Line 6
        sleepE' = L.slookup eseq' sleeps
        stEseq' = L.slookup eseq' states
        wi = wholeIndep sys stEseq' eseq' v
        w' = eseq `minus` eseq' `minus` v
    if null (intersect sleepE' wi)                   -- Line 7
    then putWuTs $ M.adjust (insertWuT sys stEseq' eseq' (L.toVar w') (L.toVar v)) eseq' wuts -- Line 8
    else return ()

minus :: Execution -> Execution -> Execution
minus l1 [] = l1
minus l1 (e:es) = 
    let l1' = delete e l1
    in minus l1' es 

exploreNext :: State ExploreState ()
exploreNext = do
    s@ExploreState{..} <- get
    forM_ (L.slookup eseq wuts) exploreNextBody -- Lines 16-22

exploreNextBody :: WakeupTreeElem -> State ExploreState ()
exploreNextBody ps = do
    s@ExploreState{..} <- get 
    let wutE   = L.slookup eseq wuts          -- wut(E)
        sleepE = L.slookup eseq sleeps        -- sleep(E)
        p      = min_prec ps -- wutE             -- Line 17: WARNING!
        sleep' = filter (models sys curstate p) sleepE -- Line 18
        wut'   = subtree p wutE               -- Line 19
    runStep p sleep' wut'      
    explore                                   -- Line 20
    putSleeps $ M.adjust (p:) eseq sleeps 
    putWuTs $ M.adjust (pruneWuT p) eseq wuts 
    putExecution eseq                    -- We need to go back because the 
    exploreNext                          -- number of iterations of Lines 16-22
                                         -- may be different.

-- run is going to execute a process 
-- and generate the updated ExploreState
-- 1. Execute the PID and generate a new current state
-- 2. Set the new sleep and wut
runStep :: PID -> SleepSet -> WakeupTree -> State ExploreState ()
runStep p sleep' wut' = do
    s@ExploreState{..} <- get
    let (curs, tags) = L.step sys curstate p
        k = L.getCount eseq p
        eseq' = eseq ++ [(p,k+1,tags)]
        states' = M.insert eseq' curs states
    put s{eseq=eseq', sleep=sleep', wut=wut', states=states', curstate=curs}

findRaces :: State ExploreState Races
findRaces = do
    s@ExploreState{..} <- get
    let (e:es) = eseq -- getEvents eseq M.empty
        pevents = pairEvents e es
    return $ filter (\(e,e') -> isRace e e' eseq) pevents

{-        
getEvents :: Execution -> M.Map Var Int -> Events
getEvents [] k = []
getEvents ((p,tags):xs) k = 
    let c = L.slookup p k
        e = (p,c+1,tags)
        k' = M.adjust (const $ c+1) p k
        es = getEvents xs k'
    in e:es
-}

pairEvents :: Event -> Events -> [(Event, Event)]
pairEvents e [] = []
pairEvents e (x:xs) = 
    let le = map ((,) e) (x:xs)
        lx = pairEvents x xs
    in le ++ lx
    
isRace :: Event -> Event -> Execution -> Bool
isRace e@(p,_,_) e'@(q,_,_) eseq = 
    p /= q && happensBefore e e' eseq && isConcurrent e e' eseq 
    
type MapPosition = M.Map Var Position
type Position = Int



isConcurrent :: Event -> Event -> Execution -> Bool
isConcurrent e@(p,i,_) e'@(q,j,_) eseq = 
    let pos = foldr (\i r -> M.insert i (elemIndices i $ fst3 $ unzip3 eseq) r) M.empty $ unique eseq
        pIndex = L.slookup p pos !! (i-1)
        qIndex = L.slookup q pos !! (j-1)
        events = map (\i -> getEvent i eseq pos) [pIndex+1 .. qIndex-1]
    in all (\e'' -> not $ happensBefore e e'' eseq && happensBefore e'' e' eseq) events
    
getEvent :: Int -> Execution -> M.Map Var [Int] -> Event
getEvent i eseq pos =
    let (p,_,tags) = eseq!!i
        lpos = L.slookup p pos
    in case elemIndex i lpos of
        Nothing -> error "Cant create event"
        Just idx -> (p,idx+1,tags)
       

-- pre(E, e) denote the prefix of E up to, 
-- but not including, the event e.
pre :: (Execution, Event) -> Execution
pre ([], e) = error "couldnt find the event"
pre ((x:xs), e) = 
    if x == e
    then []
    else let eseq' = pre (xs,e)
         in x:eseq'

-- notdep(e, E) be the sub-sequence of E consisting of the events 
-- that occur after e but do not "happen after" e.
notdep :: (Event, Execution) -> Events
notdep(e, eseq) = notdep' e eseq False
    where
      notdep' :: Event -> Events -> Bool -> Events
      notdep' e [] _ = []
      notdep' e (e':es) False = 
          if e == e'
          then notdep' e es True
          else notdep' e es False
      notdep' e (e':es) True = 
          let eseq' = notdep' e es True
          in if not (happensBefore e e' eseq)
             then e':eseq'
             else eseq'

      
-- For an execution sequence E.w, 
-- define WI[E](w) as the union of I[E](w) 
-- and the set of processes p such that p in enabled(s[E]) and E |= p <> w.
wholeIndep :: L.System -> L.State -> Execution -> Events -> [Var]
wholeIndep sys st@(val,enabledProc) eseq w = 
    let iEseq = minIndep eseq w -- I[E]
        indepProc = filter (\p -> models sys st p w) enabledProc 
    in iEseq ++ indepProc






















