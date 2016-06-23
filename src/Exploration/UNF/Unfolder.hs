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
  enevs <- foldM (\en tr -> expandWith e cevs st tr >>= \es -> return $! (es++en)) [] trs
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
    malt <- alt2 (e:d) (e:d) 
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
  -- @ASSERT The set nnevs should not contain any such event!
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
unfold = undefined

-- | expandWith adds new events to the unfolding prefix where the event *e* is 
--   an immediate predecessor.
-- expandWith returns per thread/transition, the event with the largest history
-- and necessarily an extension of the current configuration.
-- @CRITICAL
expandWith :: (Hashable st, Show act, GCS.Collapsible st act) => EventID -> EventsID -> st -> GCS.TId-> UnfolderOp st act s EventsID
expandWith e maxevs st th = do
  s@UnfolderState{..} <- get
  -- @ call the execution engine (e.g. collapse) to retrieve
  --  i. the name of the new events
  --  ii. the set of actions performed that enables us to construct
  --      a global state (i.e. the state of the configuration) and 
  --      perform sound independence/interference reasoning
  let new_events = GCS.collapse syst st th -- :: [(st,pos,[act])]
  -- @ For each triple (new_state,pos,acts) given by execution engine, 
  --   generate events with that name and actions. 
  nevs <- mapM (\(nst,pos,acts) -> extend e maxevs (th,pos) acts) new_events
  return $ concat nevs

-- | extends the unfolding prefix based on a configuration (denoted with the 
-- set of maximal events with events) of a given name which contain
-- *e* as an immediate predecessor and returns the event with history h0.
extend :: (Hashable st, Show act, GCS.Collapsible st act) => EventID -> EventsID -> EventName -> [act] -> UnfolderOp st act s EventsID
extend e maxevs êname êacts = do 
  s@UnfolderState{..} <- get
  -- @ retrieve the immediate successors of e with the same name to avoid duplicates
  succe <- lift $ get_succ e evts 
           >>= mapM (\e -> get_event "expandWith" e evts >>= \ev -> return (e,ev)) 
           >>= return . filter (\(e,ev) -> name ev == êname)
  -- @ computes h0, the maximal history:
  h0 <- computeHistory (êname,êacts) maxevs 
  if null h0
  then error $ "null h0 at expandWith(e="++show e++",name="++show êname++",maxevs="++show maxevs++")"
  else do
    -- e should be a valid maximal event
    -- @NOTE: Remove this check for benchmarking
    if e `elem` h0
    then do
      -- @ Compute other histories
      his <- if GCS.isBlocking êacts 
             then do
               prede <- lift $ predecessors e evts 
               computeHistoriesBlocking êname e prede (e `delete` h0)
             else computeHistories (êname,êacts) e [h0] [h0] 
      mapM (addEvent stak succe êname êacts) his
      addEvent stak succe êname êacts h0
    else error "e must always be in h0"  

-- | computeHistory computes the largest history of an event which we call h0 
--   Input: 
--     - Metainformation about the event (name, actions) 
--     - Set of maximal events, 
--   Output: History
-- An history is composed of events of maxevs that are dependent with (name,acts) 
-- or dependent predecessors of the independent ones that are maximal
computeHistory :: (Hashable st, GCS.Collapsible st act) => EventInfo act -> EventsID -> UnfolderOp st act s History
computeHistory einfo maxevs = do
  s@UnfolderState{..} <- get
  (maxevs_h0,maxevs') <- lift $ partition_dependent einfo evts ([],[]) maxevs 
  -- @ going up the causality of maxevs' until all maximal events are dependent with tr
  h0 <- lift $ pruneConfiguration evts maxevs_h0 maxevs'
  -- @ ASSERT: h0 does not contain repeated elements
  return h0
 where
    -- | pruneConfiguration
    --   Given a set of maximal events which are independent with transition tr
    --   go up in the causality to search for the rest of maximal events that are dependent
    pruneConfiguration events pre_his es = do
      -- immd predecessors of es
      predes <- mapM (\e -> get_pred e events) es >>= return . nub . concat
      -- filter the predecessors that are not maximal
      -- @ FIXME: Optimise this!
      -- @ NOTE: CRITICAL: THIS NEEDS TO BE CHANGED JUNE 06'16
      mpredes <- filterM (\e -> successors e events >>= \succe -> return $ null $ succe `intersect` (pre_his ++ predes)) predes
      -- split between dependent and independent 
      (pre_his',es') <- partition_dependent einfo events (pre_his,[]) mpredes
      if null es'
      then return pre_his' 
      else pruneConfiguration events pre_his' es'

-- | computeHistories : worklist algorithm 
--   Input: 
--     1. einfo: meta information regarding the event
--     2. e: event that needs to be an immediate predecessor
--     3. wlist: set of histories that we need to explore
--     4. hists: set of histories to return
--   Returns a list of histories for this meta-event where
--   the event e belongs to all of them.
--   @NOTE: Change this to be Sets
computeHistories :: (Hashable st, GCS.Collapsible st act) => EventInfo act -> EventID -> Histories -> Histories -> UnfolderOp st act s Histories
computeHistories einfo@(ename,_) e wlist hists =
  case wlist of
    [] -> return hists
    (h:hs) -> do
      s@UnfolderState{..} <- get
      -- Removes *e* and all the events from the same thread
      -- of the new event from set of events that will be 
      -- considered for pruning 
      let h' = e `delete` h
      hc <- lift $ filterM (\e' -> get_event "computeHistories" e' evts 
                        >>= \ev@Event{..} -> return $ name /= ename) h' 
      -- replace one of the maximal events with the predecessors
      -- and prune the configuration
      hs' <- mapM (computeNextHistory h) hc
      let chs = nub hs' -- remove the repeated in the new histories
          wlist' = filter (\h -> not $ h `elem` hs) chs -- filter the ones in the worklist
          nwlist = wlist' ++ hs
          nhists = filter (\h -> not $ h `elem` hists) chs -- filter the ones in the other set 
      computeHistories einfo e nwlist nhists 
 where
   -- @ computeNextHistory
   --   build a candidate history out of replacing a maximal
   --   event e' with its immediate predecessors
   computeNextHistory h e' = do
     s@UnfolderState{..} <- get
     -- we want to replace e' by its predecessors
     let h' = e' `delete` h
     -- predecessors of e'
     prede <- lift $ get_pred e' evts
     -- filter the predecessors of e' that are not maximal
     -- @ FIXME: Optimise this!
     -- @ NOTE: CRITICAL: THIS NEEDS TO BE CHANGED JUNE 06'16
     prede' <- lift $ filterM (\e -> successors e evts >>= \succe -> return $ null $ succe `intersect` h') prede
     computeHistory einfo (h'++prede')

computeHistoriesBlocking = undefined
{-
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
-}

-- @ addEvent: Given a transition id and the history
--   adds the correspondent event.
--   *Note*: We may try to add an even that is already in the prefix
--   *Flow*: 
--     1. Generate a fresh event counter: neID
--     2. Compute the set of immediate conflicts
--     3. Insert the new event in the hashtable
--     4. Update all events in the history to include neID as their successor
--     5. Update all events in the immediate conflicts to include neID as one
addEvent :: (Hashable st, GCS.Collapsible st act) => EventsID -> [(EventID,Event act)] -> EventName -> [act] -> History -> UnfolderOp st act s EventsID 
addEvent stack dup name acts history = do
  let hasDup = filter (\(e,ev) -> S.fromList (pred ev) == S.fromList history) dup
  if not $ null hasDup  
  then if length hasDup > 1
       then error "the number of duplicates is higher than 1"
       else return $ map fst hasDup 
  else do 
    s@UnfolderState{..} <- get
    -- @  a) Computes the local history of the new event
    prede <- lift $ mapM (\e -> predecessors e evts) history  
    let localHistory = prede `seq` nub $ concat prede ++ history 
        sizeLocalHistory = length localHistory + 1
    --   If we don't need cutoffs, no need to compute the linearization and the new state
    if cutoffs opts 
    then do
      let copyst = GCS.gbst syst
      gstlc <- computeStateLocalConfiguration name copyst localHistory
      if null gstlc
      then error "addEvent: the state of the local configuration is bottom"
      else do
        isCutoff <- cutoff (head gstlc) sizeLocalHistory
        if isCutoff
        then do 
          inc_cutoffs
          return []
        else add_event localHistory evts 
    else add_event localHistory evts 
 where
   add_event localHistory evts = do
     -- Increment the number of events in the prefix
     inc_evs_prefix 
     -- @ 1. Fresh event id 
     neID <- freshCounter
     -- @ 2. Compute the immediate conflicts
     -- @  b) Computes the immediate conflicts of all events in the local configuration
     lhCnfls <- lift $ foldM (\a e -> get_icnf e evts >>= \es -> return $ es ++ a) [] localHistory >>= return . nub 
     -- @  c) Compute the immediate conflicts
     cnfls <- lift $ computeConflicts name acts localHistory lhCnfls evts >>= return . nub
     -- @ 3. Insert the new event in the hash table
     let e = Event name acts history [] cnfls [] [] -- gstlc sizeLocalHistory
     lift $ set_event neID e evts 
     -- @ 4. Update all events in the history to include neID as their successor
     lift $ mapM (\e -> add_succ neID e evts) history
     -- @ 5. Update all events in the immediate conflicts to include neID as one 
     lift $ mapM (\e -> add_icnf neID e evts) cnfls 
     inc_evs_per_name name 
     return $! [neID]

computeStateLocalConfiguration = undefined
computeConflicts = undefined
{-
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
-}

-- | Compute conflicts 
-- @NOTE: @CRITICAL: THIS NEEDS TO BE OPTIMISED! 
--  DFS of the unf prefix from bottom stopping when the event:
--  . Is an immediate conflict (or successor) of an event in the local configuration
--  . Is dependent with tr
--  Changed for a worklist
compute_conflicts :: Show act => EventName -> EventsID -> EventsID -> Events s act -> ST s EventsID
compute_conflicts einfo lh lhCnfls events = do 
  ev@Event{..} <- get_event "computeConflicts" botEID events
  computeConflict [] succ 
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

-- Let e be an event of a configuration C.
--   Let cext be the conflicting extensions of C, 
--    ie. all events e' such that |-e'-| is a subset of C
--        but there is another event ê' such that e' #^ ê'
-- Let ê be the events such that e #^ ê.
-- Can ê intersect cext =!= ê? Yes. There may be immediate conflicts of 
-- e whose roots are not in C.
-- @@ compute potential alternatives @ revised: 08-04-15
computePotentialAlternatives :: Show act => EventsID -> EventsID -> UnfolderOp st act s ()
computePotentialAlternatives maxevs evs =  do
  s@UnfolderState{..} <- get
  -- @ compute the events of the configuration
  lift $ foldM_ (computePotentialAlternative stak evts) S.empty evs where

    -- @ I. V(e) where e is an event that has at least one imm conflict
    computePotentialAlternative stack events cext e =  do
      -- @ #^(e)
      cfle <- get_icnf e events
      -- @ #^(e) intersect cex(C)
      ext  <- filterM (isCExtension events stack cext) cfle
      -- @ checks if for all e' \in ext, the alternative *v* of e given by e' is valid
      de <- get_disa e events
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
        eipred <- get_pred e events
        return $! all (\p -> p `elem` confEvs) eipred

    -- @@ III. compute and verify validity of V = conf_events - e:succ e + pred e':e' 
    --    1. V is configuration: checks if e' has no conflict with any event of common 
    --    2. V respects the call stack: 
    --    3. V justifies the set of disable of e 
    computeV stack events de e e' = do
      -- @ Compute the common parts of the configurations that contain e and e': confEvs - (e:succ e) 
      clfe' <- get_icnf e' events
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
          vcfs <- mapM (\e -> get_icnf e events) v
          let justifies = all (\e -> any (\vcf -> e `elem` vcf) vcfs) de
          if justifies
          then add_alte e v events 
          else return ()
        else return () 

-- @@ filter alternatives
alt2 :: Show act => EventsID -> EventsID -> UnfolderOp st act s (Maybe Alternative)
alt2 [] _ = return Nothing
alt2 (d:ds) ods = do
  s@UnfolderState{..} <- get
  vs <- lift $ get_alte d evts
  mv <- filterM (filter_alte ods) vs
  case mv of
    [] -> alt2 ds ods
    (v:_) -> return $ Just v
 where
   -- Check if an alternative is a justification:
   --  1. The alternative justifies the disable set (i.e. there exists an immediate
   --     conflict between each event in the disable set and an event of the alternative
   --  2. The alternative is valid (i.e. there is no immediate conflict between the
   --     the stack and the alternative 
   filter_alte :: Show act => EventsID -> Alternative -> UnfolderOp st act s Bool
   filter_alte d v = do
     s@UnfolderState{..} <- get
     cnfs <- lift $ mapM (\e -> get_icnf e evts) v >>= return . nub . concat
     let isConf = not $ any (\e -> e `elem` stak) cnfs
         isJust = all (\e -> e `elem` cnfs) d
     return $ isJust && isConf 

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

