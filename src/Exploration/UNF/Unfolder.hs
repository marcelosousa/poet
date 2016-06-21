{-#LANGUAGE RecordWildCards #-}
module Exploration.UNF.Unfolder (unfolder) where

import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Data.List
import qualified Data.Map as M
import Data.Maybe hiding (catMaybes)
import qualified Data.Set as S
import Data.Set (isSubsetOf)
import qualified Data.Vector as V

import Exploration.UNF.APIStateless
import Exploration.UNF.Cutoff.McMillan
import Util.Printer (unfToDot)
import qualified Model.GCS as GCS
-- import qualified Model.Independence as I
import qualified Debug.Trace as DT

import System.IO.Unsafe
import Prelude hiding (pred)
import Util.Generic

unfolder :: (Hashable st, GCS.Collapsible st act) => Bool -> Bool -> GCS.System st act -> ST s (UnfolderState st act s)
unfolder statelessMode cutoffMode syst = do
  is <- i_unf_state statelessMode cutoffMode syst 
  (a,s) <- runStateT botExplore is 
  return $! s

-- This is the beginning of the exploration
-- where we construct the initial unfolding prefix 
-- with the bottom event
botExplore :: (Hashable st, GCS.Collapsible st act) => UnfolderOp st act s () 
botExplore = do 
  iConf <- initialExtensions 
  explore iConf botEID [] []

-- The extensions from the bottom event
-- After this function, the unfolding prefix denotes
-- the execution of bottom, and contains all extensions from it.
initialExtensions :: (Hashable st, GCS.Collapsible st act) => UnfolderOp st act s (Configuration st)
initialExtensions = do
  s@UnfolderState{..} <- get
  let e = botEID
      cevs = [e]
      st = GCS.gbst syst
      trs = GCS.enabled syst st
  enevs <- foldM (\en tr -> expandWith e cevs tr >>= \es -> return $! (es++en)) [] trs
  s@UnfolderState{..} <- get 
  let iConf = Conf st cevs enevs []
  put s{ pcnf = iConf }
  return $! iConf

separator = "-----------------------------------------\n"
-- | explore: the main exploration function
--  Input: 
--    1. c: Current configuration
--    2. ê: The latest event added to c. Necessarily a maximal event of c.
--    3. d: The set of disabled events
--    4. alt: Alternative (a corresponding branch in the wake up tree of ODPOR)
explore :: (Hashable st,GCS.Collapsible st act) => Configuration st -> EventID -> EventsID -> Alternative -> UnfolderOp st act s ()
explore c@Conf{..} ê d alt = do
  is@UnfolderState{..} <- get
  -- @ configuration is maximal?
  if null enevs 
  then do
    -- @ forall events e in Conf with immediate conflicts compute V(e)
    --   and check if its a valid alternative
    computePotentialAlternatives maevs cfevs
    inc_sum_size_max_conf 
    inc_max_conf 
  else do
    -- @ choose event to add to the current configuration
    let e = if null alt
            then head enevs
            else let lp = enevs `intersect` alt
                 in if null lp 
                    then error $ separator ++ "A `intersect` en(C) = 0 at explore(ê = " 
                                 ++ show ê ++ ", enevs = " ++ show enevs ++ ", alt = " 
                                 ++ show alt ++ ", stack = " ++ show stak ++ ")\n"
                    else head lp   
    -- @ initialize disable of e
    lift $ set_disa e d evts 
    -- @TODO [June'16] revise this! 
    pruned_conf <- 
      if stateless opts 
      then prune_config c
      else return c
    -- @ compute the new enabled events and immediate conflicts after adding *e*
    --   return the new configuration c `union` {e}
    nc <- unfold pruned_conf e
    -- @ recursive call
    push e
    explore nc e d (e `delete` alt)
    pop
    -- @ filter alternatives
    malt <- alt2 (e:d) (e:d) -- maxevs e @TODO: Why is maxevs commented?
    case malt of
      Nothing -> return ()
      Just alt' -> explore c ê (e:d) (alt' \\ stak)
    -- @ remove irrelevant parts of the prefix 
    if stateless opts 
    then do
      core_prefix <- lift $ core stak d evts
      prune e core_prefix
    else return ()

{-
-- We are going to add event e to configuration conf
-- Need to update enable, and immediateConflicts
-- computeExtensions
-- @@ unfold receives the configuration and event that is going to executed
--    and returns the new configuration with that event
--    Build the configuration step by step
-- @revised 08-04-15
unfold :: (Hashable st, GCS.Collapsible st act) => Configuration st -> EventID -> UnfolderOp st act s (Configuration st)
unfold conf@Conf{..} e = do
  s@UnfolderState{..} <- get
  tr <- lift $ getEvent "unfold" e evts >>= return . snd4 . evtr
  -- @ 1. compute the new state after executing the event e
  -- copy the state otherwise it will go wrong 
  -- copyst <- lift $ copy stc
  -- execute the event e
  nstcs <- execute cons e -- @TODO: change this
  let nstc = head nstcs
  -- @ 2. compute the new set of maximal events
  iprede <- trace (separator ++ "state = " ++ show nstc) $ lift $ getIPred e evts
  let nmaxevs = e:(mevs \\ iprede)
  -- @ 3. compute the new set of enabled events
  let es = delete e eevs 
  -- - compute the set of events independent with *e*, they will be enabled after *e*
  senevs <- lift $ filterM (\ê -> isIndependent indr e ê evts) es 
  -- - compute the set of enabled transitions at the new state
  let entrs = GCS.enabledTransitions syst nstc 
  -- - filter the enabled transitions that are dependent with h(e); those will be the new events
  --   some of these new events will be conflicting extensions
  netrs <- lift $ V.filterM (\tr -> isDependent_te indr tr e evts) entrs
  nnevs <- V.mapM (expandWith e nmaxevs) netrs >>= return . concat . V.toList 
  -- @ compute all the events of the configuration 
  let confEvs = e:stak
  -- @ filter from nnevs events that may have immediate conflicts with events in the configuration
  nnevs' <- lift $ filterM (\e -> getEvent "unfold" e evts >>= \ev -> return $ null (icnf ev `intersect` confEvs)) nnevs 
  let nenevs = nnevs' ++ senevs 
  -- @ 4. compute the new set of special events
  evs <- lift $ mapM (\e -> getEvent "unfold" e evts >>= \ev -> return (e,ev)) confEvs 
  let ncevs = map fst $ filter (\(_,ev) -> not $ null $ icnf ev) evs 
  -- @ build the new configuration
  let nconf = Conf nstc nmaxevs nenevs ncevs
  s@UnfolderState{..} <- get
  put s{ pcnf = nconf }
  return $! nconf
-}
unfold=undefined
computePotentialAlternatives = undefined
alt2 = undefined

-- expandWith only adds events that have e in the history
-- @CRITICAL
expandWith :: (Hashable st, GCS.Collapsible st act) => EventID -> EventsID -> GCS.TId-> UnfolderOp st s EventsID
expandWith e maxevs tr = do
  s@UnfolderState{..} <- get 
  -- @ retrieve the immediate successors of e with the same transition id to avoid duplicates
  succe <- lift $ getISucc e evts 
           >>= mapM (\e -> getEvent "expandWith" e evts >>= \ev -> return (e,ev)) 
           >>= return . filter (\(e,ev) -> evtr ev == tr)
  -- @ computes h0, the maximal history:
  h0 <- computeHistory maxevs tr
  if null h0
  then error $ "expandWith(e="++show e++",tr="++show tr++",h0="++show h0++",maxevs="++show maxevs++")"
  else do
    -- e should be a valid maximal event
    if e `elem` h0
    then do
      his <- if GCS.isBlocking (frd4 tr) 
             then do
               prede <- lift $ predecessors e evts 
               computeHistoriesBlocking tr e prede (e `delete` h0)
             else computeHistories tr e [h0] >>= return . nub 
      mapM (addEvent stak succe tr) his
      addEvent stak succe tr h0
    else error "e must always be in h0"  

-- @ computeHistory 
--   Input: 
--     - Set of maximal events, 
--     - Transition tr
--   Output: History
computeHistory :: (Hashable st, Ord st, GCS.Projection st) => EventsID -> GCS.TransitionInfo -> UnfolderOp st s EventsID
computeHistory maxevs tr = do
  s@UnfolderState{..} <- get
  -- set of maximal events that are dependent with tr union 
  -- they are events of maxevs that are dependent with tr 
  -- or dependent predecessors of the independent ones that are maximal
  (maxevs_h0,maxevs') <- lift $ foldM (\(l,r) e -> 
         isDependent_te indr tr e evts >>= \b -> 
            if b then return (e:l,r) else return (l,e:r)) ([],[]) maxevs
  -- @ going up the causality of maxevs' until all maximal events are dependent with tr
  pmaxevs_h0 <- lift $ pruneConfiguration indr evts maxevs_h0 tr maxevs'
  let h0 = maxevs_h0 ++ pmaxevs_h0
  return h0

-- @ pruneConfiguration
--   Given a set of maximal events which are independent with transition tr
--   go up in the causality to search for the rest of maximal events that are dependent
pruneConfiguration inde events pre_his tr es = do
  -- immd predecessors of es
  predes <- mapM (\e -> getIPred e events) es >>= return . nub . concat
  -- filter the predecessors that are not maximal
  -- @ FIXME: Optimise this!
  mpredes <- filterM (\e -> successors e events >>= \succe -> return $ null $ succe `intersect` (pre_his ++ predes)) predes
  -- split between dependent and independent 
  (es_done,es') <- foldM (\(l,r) e ->
         isDependent_te inde tr e events >>= \b -> 
            if b then return (e:l,r) else return (l,e:r)) ([],[]) mpredes
  if null es'
  then return es_done
  else do
    es_res <- pruneConfiguration inde events (pre_his ++ es_done) tr es'
    let result = nub $ es_done ++ es_res 
    return result

-- @ computeHistories : worklist algorithm 
--   e must always be a part of the histories
computeHistories :: (Hashable st, Ord st, GCS.Projection st) => GCS.TransitionInfo -> EventID -> [EventsID] -> UnfolderOp st s [EventsID]
computeHistories tr e [] = return []
computeHistories tr e (h:hs) = do
  s@UnfolderState{..} <- get
  let h' = e `delete` h
  hc <- lift $ filterM (\e' -> getEvent "computeNextHistory" e' evts >>= \ev -> return $ fst4 (evtr ev) /= fst4 tr) h' 
  -- replace one of the maximal events with the predecessors
  -- and prune the configuration
  hs' <- mapM (computeNextHistory h tr) hc
  res <- computeHistories tr e $ nub $ hs' ++ hs
  return $! hs' ++ res

-- @ computeNextHistory
--   build a candidate history out of replacing a maximal event e with its immediate predecessors
computeNextHistory :: (Hashable st, Ord st, GCS.Projection st) => EventsID -> GCS.TransitionInfo -> EventID -> UnfolderOp st s EventsID
computeNextHistory h tr e' = do
  s@UnfolderState{..} <- get
  -- we want to replace e' by its predecessors
  let h' = e' `delete` h
  -- predecessors of e'
  prede <- lift $ getIPred e' evts
  -- filter the predecessors of e' that are not maximal
  -- @ FIXME: Optimise this!
  prede' <- lift $ filterM (\e -> successors e evts >>= \succe -> return $ null $ succe `intersect` h') prede
  -- candidate
  let candidate = h' ++ prede'
  computeHistory candidate tr  

computeHistoriesBlocking ::(Hashable st, Ord st, GCS.Projection st) => GCS.TransitionInfo -> EventID -> EventsID -> EventsID -> UnfolderOp st s [EventsID]
computeHistoriesBlocking tr e _ [] = return []
computeHistoriesBlocking tr@(procID, trID, _, act) e prede [e'] = do
  s@UnfolderState{..} <- get
  ev <- lift $ getEvent "computeHistoriesBlocking" e evts
  if fst4 (evtr ev) == procID
  then do -- @ proc(e) == tr. Hence e' must be a blocking event, namely an unlock
    ev' <- lift $ getEvent "computeHistoriesBlocking" e' evts
    let acte' = frd4 $ evtr ev'
    if any (\a -> (GCS.Unlock (GCS.varOf a)) `elem` acte') act
    then do
      me'' <- findNextUnlock tr prede e'
      case me'' of
        -- @ [e] is not a valid history 
        Nothing -> return []
        -- @ [e] is a valid history
        Just [] -> return [[e]]
        -- @ [e,e''] is a valid history
        --   and by construction e'' is unlock
        Just [e''] -> do
          let hN = [e,e'']
          rest <- computeHistoriesBlocking tr e prede [e'']
          if rest == []
          then return [hN]
          else return $ hN:rest
    else error $ "computeHistoriesBlocking: can't happen! " ++ show act ++ " " ++ show acte'
  else return [] -- @ proc(e) != tr
computeHistoriesBlocking tr e es hs = error $ "computeHistoriesBlocking fatal :" ++ show (tr,e,es,hs)

--
findNextUnlock :: (Hashable st, Ord st, GCS.Projection st) => GCS.TransitionInfo -> EventsID -> EventID -> UnfolderOp st s (Maybe EventsID)
findNextUnlock tr@(_,_,_,act) prede e' = do
  s@UnfolderState{..} <- get
  iprede' <- lift $ getIPred e' evts
  (es_done,es) <- lift $ foldM (\(l,r) e ->
         isDependent_te indr tr e evts >>= \b -> 
            if b then return (e:l,r) else return (l,e:r)) ([],[]) iprede'
  -- @ es_done is either empty or has one event
  case es_done of 
    [] -> do
      lres <- mapM (findNextUnlock tr prede) es >>= return . catMaybes
      case lres of
        Nothing -> return Nothing
        Just [] -> return $ Just []
        Just [x] -> return $ Just x
        _ -> error "findNextUnlock: not sure what happens here!"
    [e] -> do
      ev <- lift $ getEvent "computeHistoriesBlocking" e evts
      let acte = frd4 $ evtr ev
      if any (\a -> (GCS.Lock (GCS.varOf a)) `elem` acte) act
      then if e `elem` prede
           then return Nothing
           else findNextUnlock tr prede e
      else if any (\a -> (GCS.Unlock (GCS.varOf a)) `elem` acte) act
           then if e `elem` prede
                then return $ Just []
                else return $ Just [e]
           else do
             let iacts = GCS.initialActs syst
             if any (\a -> (GCS.Lock (GCS.varOf a)) `elem` iacts) act
             then return Nothing
             else return $ Just []
--               trace ("returning nothing") $ return Nothing -- REVISE: error $ "findNextUnlock: cant happen! " ++ show e
    _ -> error "findNextUnlock: two events are dependent and that can't happen"


-- @ addEvent: Given a transition id and the history
--   adds the correspondent event.
--   *Note*: We may try to add an even that is already in the prefix
--   *Flow*: 
--     1. Generate a fresh event counter: neID
--     2. Compute the set of immediate conflicts
--     3. Insert the new event in the hashtable
--     4. Update all events in the history to include neID as their successor
--     5. Update all events in the immediate conflicts to include neID as one
addEvent :: (Hashable st, Ord st, Show st, GCS.Projection st) => EventsID -> [(EventID,Event)] -> GCS.TransitionInfo -> EventsID -> UnfolderOp st s EventsID 
addEvent stack dup tr history = do
  let hasDup = filter (\(e,ev) -> S.fromList (pred ev) == S.fromList history) dup
  if null hasDup  
  then do 
    s@UnfolderState{..} <- get
    -- @  a) Computes the local history of the new event
    prede <- lift $ mapM (\e -> predecessors e evts) history  
    let localHistory = prede `seq` nub $ concat prede ++ history 
        sizeLocalHistory = length localHistory + 1
    --   If we don't need cutoffs, no need to compute the linearization and the new state
    if cutoffMode
    then do
      let copyst = GCS.initialState syst
      gstlc <- computeStateLocalConfiguration tr copyst localHistory
      if null gstlc
      then return []
      else do
        isCutoff <- cutoff (head gstlc) sizeLocalHistory
        if isCutoff
        then do 
          incCutoffCounter
          return []
        else do
          incSize
          -- @ 1. Fresh event id 
          neID <- freshCounter
          -- @ 2. Compute the immediate conflicts
          -- @  b) Computes the immediate conflicts of all events in the local configuration
          lhCnfls <- lift $ foldM (\a e -> getImmediateConflicts e evts >>= \es -> return $ es ++ a) [] localHistory >>= return . nub 
          -- @  c) Compute the immediate conflicts
          cnfls <- lift $ computeConflicts indr tr localHistory lhCnfls evts >>= return . nub
          -- @ 3. Insert the new event in the hash table
          let e = Event tr history [] cnfls [] [] -- gstlc sizeLocalHistory
          lift $ setEvent neID e evts 
          -- @ 4. Update all events in the history to include neID as their successor
          lift $ mapM (\e -> setSuccessor neID e evts) history
          -- @ 5. Update all events in the immediate conflicts to include neID as one 
          lift $ mapM (\e -> setConflict neID e evts) cnfls
          incSizeTr tr
          return $! [neID]
    else do
        incSize
        -- @ 1. Fresh event id 
        neID <- freshCounter
        -- @ 2. Compute the immediate conflicts
        -- @  b) Computes the immediate conflicts of all events in the local configuration
        lhCnfls <- lift $ foldM (\a e -> getImmediateConflicts e evts >>= \es -> return $ es ++ a) [] localHistory >>= return . nub 
        -- @  c) Compute the immediate conflicts
        cnfls <- lift $ computeConflicts indr tr localHistory lhCnfls evts >>= return . nub
        -- @ 3. Insert the new event in the hash table
        let e = Event tr history [] cnfls [] [] -- gstlc sizeLocalHistory
        lift $ setEvent neID e evts 
        -- @ 4. Update all events in the history to include neID as their successor
        lift $ mapM (\e -> setSuccessor neID e evts) history
        -- @ 5. Update all events in the immediate conflicts to include neID as one 
        lift $ mapM (\e -> setConflict neID e evts) cnfls 
        incSizeTr tr
        return $! [neID]
  else return $! map fst hasDup 

-- @ Compute the global state of the local configuration
--   by doing a topological sorting
-- getISucc :: EventID -> Events s -> ST s EventsID
-- execute :: GCS.Sigma s -> EventID -> UnfolderOp s (GCS.Sigma s)
computeStateLocalConfiguration :: (Hashable st, Show st, Ord st, GCS.Projection st) => GCS.TransitionInfo -> st -> EventsID -> UnfolderOp st s [st]
computeStateLocalConfiguration (_,trID,_,i) ist prede = do
  s@UnfolderState{..} <- get
  st' <- computeStateHistory ist [0] [] prede
  let fn = GCS.getTransitionWithID syst trID
  return $! fn st'
  
computeStateHistory :: (Hashable st, Ord st, GCS.Projection st) => st -> EventsID -> EventsID -> EventsID -> UnfolderOp st s st
computeStateHistory cst [] _ _ = return cst
computeStateHistory cst (ce:rest) l prede = do
  s@UnfolderState{..} <- get
  cesucc <- lift $ getISucc ce evts
  let l' = ce:l -- events already processed
      chosen = filter (\e -> e `elem` cesucc) prede
  chosen' <- lift $ filterM (\e -> getIPred e evts >>= \prede -> return $ prede \\ l' == []) chosen
  let rest' = rest ++ chosen'
  nst <- if ce == 0 
         then return $ [cst]
         else execute cst ce
  computeStateHistory (head nst) rest' l' prede     


-- @ Compute conflicts  
--  DFS of the unf prefix from bottom stopping when the event:
--  . Is an immediate conflict (or successor) of an event in the local configuration
--  . Is dependent with tr
--  Changed for a worklist
computeConflicts :: I.UIndep -> GCS.TransitionInfo -> EventsID -> EventsID -> Events s -> ST s EventsID
computeConflicts uidep tr lh lhCnfls events = do 
  ev@Event{..} <- getEvent "computeConflicts" botEID events
  computeConflict [] succ 
  -- foldM (\a e -> computeConflict e >>= \es -> return $ es ++ a) [] succ
  where 
    computeConflict seen [] = return []
    computeConflict seen (e:rest) = do
      if e `elem` seen
      then computeConflict seen rest 
      else if e `elem` lhCnfls 
           then computeConflict (e:seen) rest -- return []
           else do
             ev@Event{..} <- getEvent "computeConflict" e events
             if not (e `elem` lh) && I.isDependent uidep tr evtr
             then do 
               lhe <- predecessors e events
               if any (\e -> elem e lhe) lhCnfls 
               then computeConflict (e:seen) rest
               else computeConflict (e:seen) rest >>= \es -> return $! e:es
             else computeConflict (e:seen) (nub $ rest ++ succ)
                  -- foldM (\a e -> computeConflict e >>= \es -> return $ es ++ a) [] succ

-- Let e be an event of a configuration C.
--   Let cext be the conflicting extensions of C, 
--    ie. all events e' such that |-e'-| is a subset of C
--        but there is another event ê' such that e' #^ ê'
-- Let ê be the events such that e #^ ê.
-- Can ê intersect cext =!= ê? Yes. There may be immediate conflicts of 
-- e whose roots are not in C.
-- @@ compute potential alternatives @ revised: 08-04-15
computePotentialAlternatives :: (Hashable st, Ord st, GCS.Projection st) => EventsID -> EventsID -> UnfolderOp st s ()
computePotentialAlternatives maxevs evs =  do
  s@UnfolderState{..} <- get
  -- @ compute the events of the configuration
  -- let confEvs = stack -- lift $ getConfEvs maxevs evts
  lift $ foldM_ (computePotentialAlternative stak evts) S.empty evs where

    -- @ I. V(e) where e is an event that has at least one imm conflict
    computePotentialAlternative stack events cext e =  do
      -- @ #^(e)
      cfle <- getImmediateConflicts e events
      -- @ #^(e) intersect cex(C)
      ext  <- filterM (isCExtension events stack cext) cfle
      -- @ checks if for all e' \in ext, the alternative *v* of e given by e' is valid
      de <- getDisabled e events
      mapM_ (computeV stack events de e) ext
      -- @ memoises the already known set of conflicting extensions 
      return $! S.union cext $ S.fromList ext
 
    -- @@ II. checks if a particular event is a conflicting extension
    --    by checking if its immediate predecessors are events of the configuration
    isCExtension events confEvs cext e = do
      -- @ checks if e is a member of the memoised set of conflicting extensions
      if S.member e cext
      then return True
      else do
        -- @ checks if the immediate predecessors of e are events of the configuration
        eipred <- getIPred e events
        return $! all (\p -> p `elem` confEvs) eipred

    -- @@ III. compute and verify validity of V = conf_events - e:succ e + pred e':e' 
    --    1. V is configuration: checks if e' has no conflict with any event of common 
    --    2. V respects the call stack: 
    --    3. V justifies the set of disable of e 
    computeV stack events de e e' = do
      -- @ Compute the common parts of the configurations that contain e and e': confEvs - (e:succ e) 
      clfe' <- getImmediateConflicts e' events
      let clfec' = filter (\e -> e `elem` stack) clfe'
          stackE = tail $ dropWhile (/=e) stack -- C(e) is a subset of V 
      if any (\e -> e `elem` stackE) clfec'
      then return () -- not stack valid
      else do 
        succes' <- mapM (\ce' -> successors ce' events >>= return . (ce':)) clfec'
        -- @ 1 and 2
        let common = stack \\ (concat succes') 
            v = common ++ [e'] 
            isStackValid = all (\e' -> e' `elem` v) stackE -- this check is probably redundant now
        if isStackValid
        then do
          -- @ Compute v: *pre-condition* prede' are in common
          -- 3. All e \in de is an immediate conflict of some e' \in v
          vcfs <- mapM (\e -> getImmediateConflicts e events) v
          let justifies = all (\e -> any (\vcf -> e `elem` vcf) vcfs) de
          if justifies
          then addAlternative e v events 
          else return ()
        else return () 

-- @@ filter alternatives
alt2 :: (Hashable st, Ord st, GCS.Projection st) => EventsID -> EventsID -> UnfolderOp st s (Maybe Alternative)
alt2 [] _ = return Nothing
alt2 (d:ds) ods = do
  s@UnfolderState{..} <- get
  vs <- lift $ getAlternatives d evts
  mv <- filterAlternatives vs ods
  case mv of
    Nothing -> alt2 ds ods
    v -> return v
  
filterAlternatives :: (Hashable st, Ord st, GCS.Projection st) => Alternatives -> EventsID -> UnfolderOp st s (Maybe Alternative)
filterAlternatives [] _ = return Nothing
filterAlternatives (v:vs) ods = do
  mv <- filterAlternative v ods
  if mv
  then return $ Just v
  else filterAlternatives vs ods
 
filterAlternative :: (Hashable st, Ord st, GCS.Projection st) => Alternative -> EventsID -> UnfolderOp st s Bool
filterAlternative v d = do
  s@UnfolderState{..} <- get
  cnfs <- lift $ mapM (\e -> getImmediateConflicts e evts) v >>= return . concat
  let isConf = not $ any (\e -> e `elem` stak) cnfs
      isJust = all (\e -> e `elem` cnfs) d
  return $ isConf && isJust
-}

-- | STATELESS RELATED FUNCTIONS
-- | Prunes the configuration based on the current prefix
--   In the stateless mode, it is possible that previously
--   enabled events of the configuration are no longer in the
--   unfolding prefix.
--  @NOTE: Add example of this. 
prune_config :: Configuration st -> UnfolderOp st act s (Configuration st)
prune_config c@Conf{..} = do
  s@UnfolderState{..} <- get
  nenevs <- lift $ filterEvents enevs evts
  return $ c {enevs = nenevs}

-- | Computes the core of the prefix necessary to continue
--   exploration
-- @NOTE: Optimise this function using Sets.
core :: Show act => EventsID -> EventsID -> Events act s -> ST s EventsID
core conf d events = do
  let confAndD = conf ++ d
  evs <- mapM (\e -> get_event "compute_core" e events) confAndD
  let altes = concat $ concatMap alte evs 
      core_prefix = confAndD ++ altes
  return $ nub core_prefix 

-- | Prunes the unfolding prefix by potentially the event e and its alternatives
prune :: Show act => EventID -> EventsID -> UnfolderOp st act s ()
prune e core = do
  s@UnfolderState{..} <- get
  ev@Event{..} <- lift $ get_event "prune" e evts
  if e `elem` core
  then lift $ reset_alte e evts
  else lift $ del_event e evts
  lift $ mapM_ (\v -> mapM_ (\e -> if e `elem` core then return () else del_event e evts) v) alte

