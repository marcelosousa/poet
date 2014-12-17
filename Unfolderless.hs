{-#LANGUAGE RecordWildCards #-}
module Unfolderless where

import Control.Monad.State.Strict
import Control.Monad.ST.Safe

import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Set (isSubsetOf)
import qualified Data.Vector as V

import APIStateless
import qualified Model as ML

--import Examples
--import Benchmark

import System.IO.Unsafe

stateless :: ML.System s -> ML.UIndep -> ST s (UnfolderState s)
stateless sys indep = do
  is <- iState sys indep 
  (a,s) <- runStateT botExplore is 
  return s

-- This is the beginning of the exploration
-- where we construct the initial unfolding prefix 
-- with the bottom event
botExplore :: UnfolderOp s () 
botExplore = do 
  iConf <- initialExtensions 
  explore iConf botEID []

-- The extensions from the the bottom event
-- After this function, the unfolding prefix denotes
-- the execution of bottom, and contains all extensions from it.
initialExtensions :: UnfolderOp s (Configuration s)
initialExtensions = do
  s@UnfolderState{..} <- get
  let e = botEID
      cevs = [e]
      st = ML.initialState syst
  trs <- lift $ ML.enabledTransitions syst st
  enevs <- V.foldM (\en tr -> expandWith e [] cevs tr >>= \es -> return (es++en)) [] trs
  s@UnfolderState{..} <- trace ("enabled after e=" ++ show e ++ " are " ++ show enevs) get
  let iConf = Conf st cevs enevs []
  put s{ pcnf = iConf }
  return iConf

separator = "\n-----------------------------------------\n"
-- @@ main function 
explore :: Configuration s -> EventID -> Alternative -> UnfolderOp s ()
explore c@Conf{..} ê alt = do
  is@UnfolderState{..} <- get
  evtstr <- lift $ showEvents evts
  trace (separator ++ "explore: ê = " ++ show ê ++ " with alt = " ++ show alt ++ "\n" ++ evtstr) $ return () 
  k <- return $ unsafePerformIO getChar
  -- @ configuration is maximal?
  if seq k $ null enevs 
  --if null enevs 
  then trace "maximal configuration" $ do
    -- @ forall special events e in the configuration compute V(e)
    computePotentialAlternatives maxevs cevs 
    return () 
  else do
    -- @ pick an event that is enabled
    let (dir,e) = if null alt
                  then (True, head enevs)
                  else trace ("2: ê "++ show ê ++ " alt:" ++ show alt) $ (False, head $ enevs `intersect` alt)
        alt' = alt \\ [e]
        choice = dir || null alt'
    -- @ initialize disable of e
    trace ("not maximal, enabled=" ++ show enevs ++ ", picked event_id="++show e) $ lift $ initializeDisabled evts e ê
    -- @ compute the new enabled events and immediate conflicts after adding *e*
    --   return a new configuration
    nc <- if choice then unfold c e else advance c e
    -- @ recursive call
    explore nc e alt'

    -- @ TODO! Stateless: Garbage collection
    setPreviousConfiguration pcnf
    -- let s' = gc s

    s@UnfolderState{..} <- get
    evtstr <- lift $ showEvents evts
    trace (separator ++ "after explore: ê = " ++ show ê ++ " e = " ++ show e ++ "\n" ++ evtstr ++ "going to filter alternatives now\n") $ return ()
    k <- return $ unsafePerformIO getChar

    -- @ filter alternatives
    malt <- seq k $ filterAlternatives maxevs e
    --malt <- filterAlternatives maxevs e
    if null malt
    then return () 
    else do
      let alt' = head malt
      lift $ addDisabled e ê evts
      evs <- lift $ getConfEvs maxevs evts
      explore c ê (alt' \\ evs)

-- We are simply going to advance because alt \\ [e] is not empty
advance :: Configuration s -> EventID -> UnfolderOp s (Configuration s)
advance conf@Conf{..} e = trace ("advance with " ++ show e) $ do
  s@UnfolderState{..} <- get
  -- @ 1. compute the new state after executing the event e
  -- copy the state otherwise it will go wrong 
  copyst <- lift $ ML.copy stc
  -- execute the event e
  nstc <- execute copyst e
  -- @ 2. compute the new set of maximal events
  iprede <- lift $ getIPred e evts
  let nmaxevs = e:(maxevs \\ iprede)
  -- @ 3. compute the new set of special events
  ev@Event{..} <- lift $ getEvent "advance" e evts
  let ncevs = if null icnf then cevs else e:cevs
  -- @ 4. compute the new set of enabled events
  --    . simply the set of successors of *e*
  --   some of these new events will be conflicting extensions
  let es = delete e enevs 
  senevs <- lift $ filterM (\ê -> isIndependent inde e ê evts) es 
  nnevs <- lift $ successors e evts
  let nenevs = nnevs ++ senevs
  if null nenevs
  then error "advance: "
  else do  
    -- @ build the new configuration
    let nconf = Conf nstc nmaxevs nenevs ncevs
    s@UnfolderState{..} <- get
    put s{ pcnf = nconf }
    return nconf

-- We are going to add event e to configuration conf
-- Need to update enable, and immediateConflicts
-- computeExtensions
-- @@ unfold receives the configuration and event that is going to executed
--    and returns the new configuration with that event
--    Build the configuration step by step
unfold :: Configuration s -> EventID -> UnfolderOp s (Configuration s)
unfold conf@Conf{..} e = trace ("unfold with " ++ show e) $ do
  s@UnfolderState{..} <- get
  -- @ 1. compute the new state after executing the event e
  -- copy the state otherwise it will go wrong 
  copyst <- lift $ ML.copy stc
  -- execute the event e
  nstc <- execute copyst e
  -- @ 2. compute the new set of maximal events
  iprede <- lift $ getIPred e evts
  let nmaxevs = e:(maxevs \\ iprede)
  -- @ 3. compute the new set of special events
  ev@Event{..} <- lift $ getEvent "unfold" e evts
  let ncevs = if null icnf then cevs else e:cevs
  -- @ 4. compute the new set of enabled events
  let es = delete e enevs 
  -- - compute the set of events independent with *e*, they will be enabled after *e*
  senevs <- lift $ filterM (\ê -> isIndependent inde e ê evts) es 
  -- - compute the set of enabled transitions at the new state
  entrs <- lift $ ML.enabledTransitions syst nstc 
  -- - filter the enabled transitions that are dependent with h(e); those will be the new events
  --   some of these new events will be conflicting extensions
  netrs <- lift $ V.filterM (\tr -> isDependent_te inde tr e evts) entrs
  nnevs <- V.mapM (expandWith e maxevs nmaxevs) netrs >>= return . concat . V.toList 
  let nenevs = nnevs ++ senevs 
  -- @ build the new configuration
  let nconf = Conf nstc nmaxevs nenevs ncevs
  s@UnfolderState{..} <- get
  put s{ pcnf = nconf }
  return nconf
    
-- expandWith only adds events that have e in the history
expandWith :: EventID -> EventsID -> EventsID -> ML.TransitionID -> UnfolderOp s EventsID
expandWith e omaxevs maxevs tr = trace ("expandWith: " ++ show e) $ do
  s@UnfolderState{..} <- get 
  -- @ computes the history: set of maximal events that are dependent with tr
  history <- lift $ filterM (\e -> isDependent_te inde tr e evts) maxevs
  if null history
  then trace (show tr ++ " has empty history with max_events: " ++ show maxevs) $ return [] 
  else do
    -- e should be a valid maximal event
    if e `elem` history
    then if history == [e]
         then addEvent tr history
         else do
           -- @ compute all possible histories based on the assumption that
           --   a transition cannot disable another transition. 
           --   This means that if *e* is the event that enabled tr (to be checked below),
           --   then all subsequences of the history which contain *e* are valid histories.
           --   If *e* is not the event that enabled tr, then there must be another event *e'*
           --   immediate conflict of *e* where h(e') = tr, and whose immediate predecessors
           --   are contained in the set of maximal events. 
           let history' = e `delete` history
               histories' = map (e:) $ subsequences history' 
               histories = history `delete` histories'
           -- @ Not all subsequences are valid histories 
           --   because *tr* may only be enabled by 
           --   a certain set of events subset of maxevs
           --  . retrieve the set of immediate predecessors of e
           es' <- lift $ getImmediateConflicts e evts
           --  . filter es' of evtr == tr
           estr <- lift $ filterM (\e' -> getEvent "unfold" e' evts >>= \ev -> return $ evtr ev == tr) es'
           --  . check for null estr: if so all subsequences are valid. otherwise potentially need to filter
           if null estr 
           then mapM (addEvent tr) histories
           else do
             -- for each conflicting event with the same tr compute the immediate predecessors
             -- to verify if they are maximal events. Hence, their counter-part event needs to be added  
             estrp <- lift $ mapM (\e -> getIPred e evts) estr
             -- filter the histories from different configurations
             estrs <- lift $ filterM (allM (\p -> enables p omaxevs tr evts)) estrp
             if null estrs
             -- all subsequences are valid 
             then mapM (addEvent tr) histories
             -- not all subsequences are valid 
             else do
               -- update estrs for the new maxevs
               -- compute the maximal events that were removed
               let rmax = omaxevs \\ maxevs
                   -- replace old maximal events by e 
                   uestrs = map (\h -> nub $ map (\e' -> if e' `elem` rmax then e else e') h) estrs
                   -- filter histories that contain one of uestrs
                   fhistories = filter (\h -> any (\h' -> all (\e -> e `elem` h') h) uestrs) histories
               mapM (addEvent tr) fhistories 
           addEvent tr history
    else error "Unreachability to be proved"  
  where
    -- @ checks if an event enables the transition
    enables e maxevs tr events = do
      if e `elem` maxevs 
      then do 
        ce <- getImmediateConflicts e events >>= mapM (\e -> getEvent "enables" e events)
        return $ all (\ev -> evtr ev /= tr) ce 
      else return False
-- @ addEvent: Given a transition id and the history
--   adds the correspondent event.
--   *Pre-condition*: The event to be added is not in the unf prefix
--   *Flow*: 
--     1. Generate a fresh event counter: neID
--     2. Compute the set of immediate conflicts
--     3. Insert the new event in the hashtable
--     4. Update all events in the history to include neID as their successor
--     5. Update all events in the immediate conflicts to include neID as one
addEvent :: ML.TransitionID -> EventsID -> UnfolderOp s EventsID 
addEvent tr history = trace ("addEvent with tr_id=" ++ show tr) $ do
  s@UnfolderState{..} <- get
  -- @ 1. Fresh event id 
  neID <- freshCounter
  -- @ 2. Compute the immediate conflicts
  -- @  a) Computes the local history of the new event 
  prede <- trace ("adding event " ++ show neID ++ " with history " ++ show history) lift $ mapM (\e -> predecessors e evts) history        
  let localHistory = prede `seq` nub $ concat prede ++ history 
  -- @  b) Computes the immediate conflicts of all events in the local configuration
  lhCnfls <- lift $ foldM (\a e -> getImmediateConflicts e evts >>= \es -> return $ es ++ a) [] localHistory >>= return . nub 
  -- @  c) Compute the immediate conflicts
  cnfls <- lift $ computeConflicts inde tr localHistory lhCnfls evts >>= return . nub
  -- @ 3. Insert the new event in the hash table
  let e = Event tr history [] cnfls [] []
  lift $ setEvent neID e evts 
  -- @ 4. Update all events in the history to include neID as their successor
  lift $ mapM (\e -> setSuccessor neID e evts) history
  -- @ 5. Update all events in the immediate conflicts to include neID as one 
  lift $ mapM (\e -> setConflict neID e evts) cnfls 
  return [neID]

-- @ Compute conflicts  
--  DFS of the unf prefix from bottom stopping when the event:
--  . Is in the local configuration
--  . Is an immediate conflict of an event in the local configuration
--  . Is dependent with tr
computeConflicts :: ML.UIndep -> ML.TransitionID -> EventsID -> EventsID -> Events s -> ST s EventsID
computeConflicts uidep tr lh lhCnfls events = do 
  ev@Event{..} <- getEvent "computeConflicts" botEID events
  foldM (\a e -> computeConflict e >>= \es -> return $ es ++ a) [] succ
  where 
    computeConflict e = 
      if e `elem` lh || e `elem` lhCnfls
      then return []
      else do
        ev@Event{..} <- getEvent "computeConflict" e events
        if ML.isDependent uidep tr evtr
        then return [e]
        else foldM (\a e -> computeConflict e >>= \es -> return $ es ++ a) [] succ

-- Let e be an event of a configuration C.
--   Let cext be the conflicting extensions of C, 
--    ie. all events e' such that |-e'-| is a subset of C
--        but there is another event ê' such that e' #^ ê'
-- Let ê be the events such that e #^ ê.
-- Can ê intersect cext =!= ê? Yes. There may be immediate conflicts of 
-- e whose roots are not in C.
-- @@ compute potential alternatives 
computePotentialAlternatives :: EventsID -> EventsID -> UnfolderOp s ()
computePotentialAlternatives maxevs evs = trace ("computePotentialAlternatives: maxevs = " ++ show maxevs ++ ", evs = " ++ show evs) $ do
  s@UnfolderState{..} <- get
  -- @ compute the events of the configuration
  confEvs <- lift $ getConfEvs maxevs evts
  trace ("all events of the configuration = " ++ show confEvs) $ lift $ foldM_ (computePotentialAlternative evts confEvs) S.empty evs where
    -- @ V(e) where e is an event that has at least one imm conflict
    computePotentialAlternative events confEvs cext e = trace ("computing pot. alt of " ++ show e) $ do
      -- @ #^(e)
      cfle <- getImmediateConflicts e events
      -- @ #^(e) intersect cex(C)
      ext  <- filterM (isCExtension events confEvs cext) cfle
      -- @ checks if for all e' \in ext, the alternative *v* of e given by e' is valid
      de <- getDisabled e events
      mapM_ (computeV events confEvs de e) ext
      -- @ memoises the already known set of conflicting extensions 
      return $ S.union cext $ S.fromList ext
    -- @@ checks if a particular event is a conflicting extension
    --    by checking if its immediate predecessors are events of the configuration
    isCExtension events confEvs cext e = do
      -- @ checks if e is a member of the memoised set of conflicting extensions
      if S.member e cext
      then return True
      else do
        -- @ checks if the immediate predecessors of e are events of the configuration
        eipred <- getIPred e events
        return $ all (\p -> p `elem` confEvs) eipred
    -- @@ compute and verify validity of V = conf_events - e:succ e + pred e':e' 
    --    1. V is configuration: checks if e' has no conflict with any event of common 
    --    2. V respects the call stack: ord(e) > ord(e' \in common) where
    --       ord denotes the moment of creation represented by the event_id 
    --    3. V justifies the set of disable of e 
    computeV events confEvs de e e' = do
      -- @ Compute the common parts of the configurations that contain e and e': confEvs - (e:succ e) 
      succe  <- successors e events
      let common = confEvs \\ (e:succe)
      clfe' <- getImmediateConflicts e' events
      -- @ 1 and 2
      let isConf = null $ intersect common clfe'
          isStackValid = all (<e) common 
      if isConf && isStackValid
      then do
        -- @ Compute v: *pre-condition* prede' are in common
        let v = common ++ [e'] 
        -- 3. All e \in de is an immediate conflict of some e' \in v
        vcfs <- mapM (\e -> getImmediateConflicts e events) v
        let justifies = all (\e -> any (\vcf -> e `elem` vcf) vcfs) de
        if justifies
        then addAlternative e v events 
        else return ()
      else return () 

-- @ initialize disabled events of *e* based on de(ê)
initializeDisabled :: Events s -> EventID -> EventID -> ST s ()
initializeDisabled events e ê = do
  dê  <- getDisabled ê events
  icê <- getImmediateConflicts ê events
  let de = dê \\ icê
  setDisabled e de events

-- @@ filter alternatives
filterAlternatives :: EventsID -> EventID -> UnfolderOp s Alternatives
filterAlternatives maxevs e = trace ("filterAlternatives with maxevs:" ++ show maxevs ++ " and e: " ++ show e) $ do 
  s@UnfolderState{..} <- get
  -- @ compute all the events of the configuration
  confEvs <- lift $ getConfEvs maxevs evts
  -- @ compute V(D(e) U e)
  de <- lift $ getDisabled e evts
  vs <- lift $ mapM (\e -> getAlternatives e evts) (e:de) >>= return . concat
  -- @ compute min(dec)
  let ede = e:de 
  edepred <- lift $ mapM (\e -> predecessors e evts) ede >>= return . S.fromList . concat
  dec' <- lift $ filterM (\e -> predecessors e evts >>= \prede -> return $ all (\e -> not $ elem e prede) ede) ede
  let dec = trace ("edepred = " ++ show edepred ) $ S.fromList dec' 
      maxevset = S.fromList maxevs
  lift $ filterM (filterAlternative evts confEvs maxevset dec) vs where
    -- @@ filter alternative
    filterAlternative events confEvs maxevset dec v = trace ("filtering alternative v = " ++ show v ++ " of e=" ++ show e ++ "  with configuration events= " ++ show confEvs) $ do
      let vset = S.fromList v
      if maxevset `isSubsetOf` vset
      then do
        cextv <- cex events confEvs v >>= return . S.fromList
        if dec `isSubsetOf` cextv
        then trace ("alt v = " ++ show v ++ " is valid") $ return True
        else trace ("alt v = " ++ show v ++ " is not valid: dec = " ++ show dec ++ " ; cextv = " ++ show cextv) $ return False 
      else return False
    -- @@ compute conflicting extensions of an alternative
    cex events confEvs v = 
      mapM (cexe events confEvs) v >>= return . concat
    -- @@ compute conflicting extensions of an event
    cexe events confEvs e = do
      -- @ #^(e)
      cfle <- getImmediateConflicts e events
      -- @ #^(e) intersect cex(C)
      filterM (isCExtension events confEvs) cfle
    -- @@ checks if a particular event is a conflicting extension
    --    by checking if its immediate predecessors are events of the configuration
    isCExtension events confEvs e = do
      -- @ checks if the immediate predecessors of e are events of the configuration
      eipred <- getIPred e events
      return $ all (\p -> p `elem` confEvs) eipred

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f [] = return True
allM f (x:xs) = do
  b <- f x
  if b
  then allM f xs
  else return False 
