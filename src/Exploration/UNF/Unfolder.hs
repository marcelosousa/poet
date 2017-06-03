{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE FlexibleContexts #-}
module Exploration.UNF.Unfolder (unfolder) where

-- import Util.Printer (unfToDot)
import Control.Monad.State.Strict
import Data.List
import Data.Maybe                      hiding (catMaybes)
import Data.Set                               (isSubsetOf)
import Domain.Action                   
import Domain.Class                    hiding (add_warns)
import Domain.Lattice
import Exploration.UNF.API
import Exploration.UNF.Cutoff.McMillan
import Exploration.UNF.State
import Language.SimpleC.Util
import Prelude                         hiding (pred)
import Util.Generic                    
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Model.GCS             as GCS
import Language.SimpleC.AST
import System.Console.ANSI

getCharDebug = return ()
clearScreenDebug = return ()

unfolder :: Domain s a => Bool -> Bool -> Int -> GCS.System s a -> IO (UnfolderState s a)
unfolder stl cut wid syst = do 
  showMStr $ "UNFOLDER BEGIN:\n" ++ show_symt (GCS.symt syst) 
  getCharDebug
  clearScreenDebug
  is    <- i_unf_state stl cut wid syst 
  (a,s) <- runStateT bot_explore is 
  return $! s

-- This is the beginning of the exploration
-- where we construct the initial unfolding prefix 
-- with the bottom event
bot_explore :: Domain st act => UnfolderOp st act () 
bot_explore = do 
  iConf <- initial_ext
  explore iConf botEID [] []

-- The extensions from the bottom event
-- After this function, the unfolding prefix denotes
-- the execution of bottom, and contains all extensions from it.
initial_ext :: Domain st act => UnfolderOp st act (Configuration st)
initial_ext = do
  lift $ showMStr $ "EXTENSIONS FROM BOTTOM BEGIN"  
  s@UnfolderState{..} <- get
  let e    = botEID
      cevs = [e]
      st   = GCS.gbst syst
      trs  = fst $ unzip $ enabled syst st
  lift $ showMStr ("initial_ext: enabled threads " ++ show trs)
  enevs <- foldM (\en tr -> extend e cevs st tr >>= \es -> return $! (es++en)) [] trs
  s@UnfolderState{..} <- get 
  let iConf = Conf st cevs enevs []
  put s{ pcnf = iConf }
  lift $ showMStr $ "EXTENSIONS FROM BOTTOM END"  
  _ <- lift $ getCharDebug
  return $! iConf

separator = "-----------------------------------------\n"

-- | explore: the main exploration function
--  Input: 
--    1. c: Current configuration
--    2. ê: The latest event added to c. Necessarily a maximal event of c.
--    3. d: The set of disabled events
--    4. alt: Alternative (a corresponding branch in the wake up tree of ODPOR)
explore :: Domain st act => Configuration st -> EventID -> EventsID -> Alternative -> UnfolderOp st act ()
explore c@Conf{..} ê d alt = do
  is@UnfolderState{..} <- get
  str <- lift $ showEvents evts
  lift $ clearScreenDebug
  lift $ showMStr $ (separator ++ "explore(ê = " ++ show ê ++ ", d = " ++ show d 
       ++ ", enevs = " ++ show enevs ++ ", alt = " 
       ++ show alt ++ ", stack = " ++ show stak
       ++")\n"++show state++"\nEvents in the Prefix\n"++str++"\n"++separator) 
  k <- lift $ getCharDebug
  -- @ configuration is maximal?
  -- if null enevs 
  if null enevs 
  then do
    -- @ forall events e in Conf with immediate conflicts compute V(e)
    --   and check if its a valid alternative
    lift $ showMStr $ "explore: maximal configuration" 
    possible_altes maevs cfevs
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
    lift $ showMStr $ "explore: calling unfold(" ++ show e ++ ")" 
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

-- We are going to add event e to configuration conf
-- Need to update enable, and immediateConflicts
-- computeExtensions
-- @@ unfold receives the configuration and event that is going to executed
--    and returns the new configuration with that event
--    Build the configuration step by step
-- @revised 08-04-15
unfold :: Domain st act => Configuration st -> EventID -> UnfolderOp st act (Configuration st)
unfold conf@Conf{..} e = do
  s@UnfolderState{..} <- get
  -- @ 1. compute the new state after executing the event e
  lift $ showMStr $ "unfold: going to execute(" ++ show e ++ ")"
  nstc   <- execute state e
  -- @ 2. compute the new set of maximal events
  iprede <- lift $ get_pred e evts 
  let nmaxevs = e:(maevs \\ iprede)
  -- @ 3. compute the new set of enabled events
      es = delete e enevs 
  -- - compute the set of events independent with *e*, they will be enabled after *e*
  nstc `seq` lift $ showMStr ("unfold: event = " ++ show e ++ ", enabled events = " ++ show es ++ ", new maximal evs = " ++ show nmaxevs)
  senevs <- lift $ filterM (\ê -> is_independent e ê evts) es
  -- - get the tid of these independent events
  itrs   <- lift $ mapM (\e -> get_tid e evts) senevs 
  -- - compute the set of enabled threads at the new state
  lift $ showMStr ("unfold: computing enabled threads at state\n" ++ show nstc)
  let entrs = fst $ unzip $ enabled syst nstc 
  -- - filter the enabled threads that are dependent with h(e); 
      netrs = entrs \\ itrs 
  lift $ showMStr $ "unfold: enabled threads = " ++ show entrs
  lift $ showMStr $ "unfold: independent enabled events = " ++ show senevs
  lift $ showMStr $ "unfold: new events of threads = " ++ show netrs
  _     <- lift $ getCharDebug
  nnevs <- mapM (extend e nmaxevs nstc) netrs >>= return . concat  
  -- @ compute all the events of the configuration 
  let confEvs = e:stak
  -- @ filter from nnevs events that may have immediate conflicts with events in the config
  -- @ASSERT The set nnevs should not contain any such event!
  nnevs' <- lift $ filterM (\e -> get_event "unfold" e evts >>= 
                   \ev -> return $ null (icnf ev `intersect` confEvs)) nnevs 
  let nenevs = nnevs' ++ senevs 
  -- @ 4. compute the new set of special events
  evs <- lift $ mapM (\e -> get_event "unfold" e evts >>= \ev -> return (e,ev)) confEvs 
  let ncevs = map fst $ filter (\(_,ev) -> not $ null $ icnf ev) evs 
  -- @ build the new configuration
  let nconf = Conf nstc nmaxevs nenevs ncevs
  s@UnfolderState{..} <- get
  put s{ pcnf = nconf }
  return $! nconf
 where 
   -- | execute receives a state and an event id and:
   --   runs the execution engine on this state based on the event
   --   gets the new state and the new set of actions
   --   updates the event based on this set of actions
   --   returns the new state
   execute :: Domain st act => st -> EventID -> UnfolderOp st act st 
   execute st e = do
     s@UnfolderState{..} <- get
     ev@Event{..} <- lift $ get_event "execute" e evts
     lift $ showMStr $ "execute: going to call drun" ++ show name
     let (nst, nacts) = drun (widening opts) syst st name
         nev = ev {acts = nacts}
     lift $ set_event e nev evts
     return nst 

-- | extend adds new events to the unfolding prefix where the event *e* is 
--   an immediate predecessor.
-- extend returns per thread/transition, the event with the largest history
-- and necessarily an extension of the current configuration.
extend :: Domain st act => EventID -> EventsID -> st -> GCS.TId -> UnfolderOp st act EventsID
extend e maxevs st th = do
  lift $ showMStr ("extend: e = " ++ show e ++ ", maxevs = " ++ show maxevs ++ ", th = " ++ show th)
  s@UnfolderState{..} <- get
  -- @ call the execution engine (e.g. collapse) to retrieve
  --  i. the name of the new events
  --  ii. the set of actions performed that enables us to construct
  --      a global state (i.e. the state of the configuration) and 
  --      perform sound independence/interference reasoning
  lift $ showMStr "extend: calling collapse"
  let (warns,new_events) = run False (widening opts) syst st th 
  new_events `seq` lift $ showMStr ("extend: collapse result\n   " ++ show (map (\(a,b,c) -> (b,c)) new_events))
  add_warns warns
  -- @ For each triple (new_state,pos,acts) given by execution engine, 
  --   generate events with that name and actions. 
  lift $ showMStr $ "extend: calling ext(" ++ show e ++ ")"
  nevs <- mapM (\(nst,pos,acts) -> ext e maxevs nst (th,pos) acts) new_events
  return $ concat nevs

-- | extends the unfolding prefix based on a configuration (denoted with the 
-- set of maximal events with events) of a given name which contain
-- *e* as an immediate predecessor and returns the event with history h0.
ext :: (Show act, Domain st act) => EventID -> EventsID -> st -> (GCS.TId, GCS.Pos) -> act -> UnfolderOp st act EventsID
ext e maxevs st (tid,pos) êacts = do 
  _ <- lift $ getCharDebug
  let tid_sym = GCS.toThCFGSym st tid 
      êname = (tid,pos,tid_sym)
  lift $ showMStr $ "ext: extensions with name " ++ show êname 
  s@UnfolderState{..} <- get
  -- @ retrieve the immediate successors of e with the same name to avoid duplicates
  succe <- lift $ get_succ e evts 
           >>= mapM (\e -> get_event "ext" e evts >>= \ev -> return (e,ev)) 
           >>= return . filter (\(e,ev) -> name ev == êname)
  -- @ computes h0, the maximal history:
  lift $ showMStr $ "ext: calling history to compute the enabled extension (h0)"
  h0 <- history (êname,êacts) maxevs 
  _ <- lift $ getCharDebug
  if null h0
  then error $ "null h0 at expandWith(e="++show e
             ++",name="++show êname++",maxevs="++show maxevs++")"
  else do
    -- e should be a valid maximal event
    -- @NOTE: Remove this check for benchmarking
    if e `elem` h0
    then do
      -- @ Compute other histories
      lift $ showMStr $ "ext: computing conflicting extensions"
      -- @ If the action is a JOIN action, then we dont need to compute conflicting extensions 
      his <- if isJoin êacts || isUnlock êacts
             then return []
             else  
               if isLock êacts 
               then do
                 -- @ Compute histories related to locks
                 lift $ showMStr $ "ext: calling histories_lock"
                 histories_lock (êname,êacts) e h0 
               else do 
                 lift $ showMStr $ "ext: calling histories"
                 histories (êname,êacts) e [h0] [] 
      his `seq` lift $ showMStr $ "ext: conflicting extensions histories = " ++ show his
      _ <- lift getCharDebug
      lift $ showMStr "ext: adding conflicting extensions events"
      mapM (add_event False stak succe êname êacts) his
      lift $ showMStr "ext: finished adding conflicting extensions events"
      lift $ showMStr "ext: adding the enabled event"
      add_event True stak succe êname êacts h0
    else return [] -- error "e must always be in h0"  

-- | history computes the largest history of an event which we call h0 
--   Input: 
--     - Metainformation about the event (name, actions) 
--     - Set of maximal events, 
--   Output: History
-- An history is composed of events of maxevs that are dependent with (name,acts) 
-- or dependent predecessors of the independent ones that are maximal
history :: (Domain st act) => EventInfo act -> EventsID -> UnfolderOp st act History
history einfo@(ename,eacts) maxevs = do
  lift $ showMStr ("history: ename = " ++ show ename ++ "\n\t eacts = " ++ show eacts ++ "\n\t maxevs = " ++ show maxevs)
  s@UnfolderState{..} <- get
  lift $ showMStr $ "history: calling partition_dependent("++ show maxevs++")"
  lift $ showMStr $ "partition_dependent: begin"
  (maxevs_h0,maxevs') <- lift $ partition_dependent einfo evts ([],[]) maxevs 
  lift $ showMStr $ "history: partition_dependent result = (deps = "++ show maxevs_h0++", indeps = " ++ show maxevs' ++ ")"
  -- @ going up the causality of maxevs' until all maximal events are dependent with tr
  h0 <- if null maxevs' 
        then return maxevs_h0
        else do 
    lift $ showMStr $ "history: calling pruneConfiguration("++show maxevs'++")"
    lift $ pruneConfiguration evts maxevs_h0 maxevs'
  -- @ ASSERT: h0 does not contain repeated elements
  lift $ showMStr ("history: result = " ++ show h0)
  return h0
 where
    -- | pruneConfiguration
    --   Given a set of maximal events which are independent with transition tr
    --   go up in the causality to search for the rest of maximal events that are dependent
    pruneConfiguration events pre_his es = do
      -- immd predecessors of es
      prdes <- mapM (\e -> get_pred e events) es >>= return . nub . concat
      -- filter the predecessors that are not maximal
      -- @ FIXME: Optimise this!
      -- @ NOTE: CRITICAL: THIS NEEDS TO BE CHANGED JUNE 06'16
      mpredes <- filterM (\e -> successors e events >>= 
                          \succe -> return $ null $ succe `intersect` (pre_his ++ prdes)) prdes
      -- split between dependent and independent 
      showMStr $ "pruneConfiguration: calling partition_dependent("++ show mpredes++")"
      showMStr $ "partition_dependent: begin"
      (pre_his',es') <- partition_dependent einfo events (pre_his,[]) mpredes
      if null es'
      then do
        showMStr "pruneConfiguration: end" 
        return pre_his' 
      else pruneConfiguration events pre_his' es'

-- | histories : worklist algorithm 
--   Input: 
--     1. einfo: meta information regarding the event
--     2. e: event that needs to be an immediate predecessor
--     3. wlist: set of histories that we need to explore
--     4. hists: set of histories to return
--   Returns a list of histories for this meta-event where
--   the event e belongs to all of them.
--   @NOTE: Change this to be Sets
histories :: (Domain st act) => EventInfo act -> EventID -> Histories -> Histories -> UnfolderOp st act Histories
histories einfo@(ename,_) e wlist hists =
  case wlist of
    [] -> do
      let hists_res = nub hists
      lift $ showMStr $ "histories: result = " ++ show hists_res
      return hists_res
    (h:hs) -> do
      s@UnfolderState{..} <- get
      -- Removes *e* and all the events from the same thread
      -- of the new event from set of events that will be 
      -- considered for pruning 
      -- and the event that creates the thread
      -- if that is the case, then it cannot be removed
      let h' = e `delete` h
      hc <- lift $ filterM (\e' -> get_event "computeHistories" e' evts 
                        >>= \ev@Event{..} -> return $ (fst3 name /= fst3 ename && not (isCreateOf (SymId $ fst3 ename) acts))) h' 
      -- replace one of the maximal events with the predecessors
      -- and prune the configuration
      hs' <- mapM (next_history h) hc
      let chs = nub hs'                                 -- remove the repeated new histories
          wlist' = filter (\h -> not $ h `elem` hs) chs -- filter the ones in the worklist
          nwlist = wlist' ++ hs
          nhists = chs ++ hists 
      histories einfo e nwlist nhists 
 where
   -- @ next_history 
   --   build a candidate history out of replacing a maximal
   --   event e' with its immediate predecessors
   next_history h e' = do
     lift $ showMStr $ "next_history: from history = " ++ show h
     s@UnfolderState{..} <- get
     -- we want to replace e' by its predecessors
     let h' = e' `delete` h
     -- predecessors of e'
     prede <- lift $ get_pred e' evts
     -- filter the predecessors of e' that are not maximal
     -- @ FIXME: Optimise this!
     -- @ NOTE: CRITICAL: THIS NEEDS TO BE CHANGED JUNE 06'16
     prede' <- lift $ filterM (\e -> successors e evts >>= 
                               \succe -> return $ null $ succe `intersect` h') prede
     lift $ showMStr $ "next_history: calling history(" ++ show (h'++prede') ++ ")"
     history einfo (h'++prede')

-- | Going to compute the histories of a lock event.
--   This history can be composed of at most two events
--   one to enable this transition in the thread and 
--   an unlock from another thread.
histories_lock :: (Domain st act) => EventInfo act -> EventID -> EventsID -> UnfolderOp st act Histories
histories_lock info@(ne_name,ne_acts) s_e hs = do
  lift $ showMStr $ "histories_lock: ne_name = " ++ show ne_name ++ "\n\t ne_acts = " ++ show ne_acts ++ "\n\t hs = " ++ show hs
  _ <- lift getCharDebug
  case hs of
    [e] -> return []
    [e,e'] -> do 
      s@UnfolderState{..} <- get
      ev  <- lift $ get_event "histories_lock" e evts
      ev' <- lift $ get_event "histories_lock" e' evts
      let acte = acts ev
          acte' = acts ev'
          (e_unlk, e_en) =
            if isUnlockOf ne_acts acte
            then 
               mytrace False ("histories_lock: e is an unlock " ++ show e) $
               if isUnlockOf ne_acts acte'
               then error $ "histories_lock: both events in the history are unlocks"
               else (e, e') 
            else if isUnlockOf ne_acts acte'
                 then (e', e)
                 else error $ "histories_lock: none of the events in the history is an unlock"
      if e_unlk == s_e
      then do
        lift $ showMStr $ "histories_lock: the last event is the unlock (skipping)"
        return [] 
      else do
        lift $ showMStr $ "histories_lock: the event " ++ show e_unlk ++ " is an unlock"
        -- Get the local configuration of both e_en and e_unlk
        pred_e_en <- lift $ predecessors e_en evts
        pred_e_unlk <- lift $ predecessors e_unlk evts
        -- @TODO: Check if this is the intersecting the configurations properly?
        let c_e_unlks = pred_e_unlk \\ (e_en:pred_e_en)
        lift $ showMStr $ "histories_lock: candidate unlocks = " ++ show c_e_unlks
        e_unlks <- filterM (\c_e_unlk -> do 
            c_ev_unlk <- lift $ get_event "histories_lock" c_e_unlk evts  
            return $ isUnlockOf ne_acts (acts c_ev_unlk)) c_e_unlks 
        if null e_unlks
        then return [[e_en]]
        else return $ map (\e -> [e,e_en]) e_unlks
    _ -> error $ "histories_lock: too many events in the history " ++ show hs 

-- @ add_event: Given a transition id and the history
--   adds the correspondent event.
--   *Note*: We may try to add an even that is already in the prefix
--   *Flow*: 
--     1. Generate a fresh event counter: neID
--     2. Compute the set of immediate conflicts
--     3. Insert the new event in the hashtable
--     4. Update all events in the history to include neID as their successor
--     5. Update all events in the immediate conflicts to include neID as one
add_event :: (Domain st act) => Bool -> EventsID -> [(EventID,Event act)] -> EventName -> act -> History -> UnfolderOp st act EventsID 
add_event is_in_conf stack dup name acts history = do
  lift $ showMStr ("add_event: name = " ++ show name ++ ", hist = " ++ show history) 
  _ <- lift $ getCharDebug
  let hasDup = filter (\(e,ev) -> S.fromList (pred ev) == S.fromList history) dup
  if not $ null hasDup  
  then if length hasDup > 1
       then error "add_event: the number of duplicates is higher than 1"
       else do 
         lift $ showMStr "add_event: event was already in the prefix" 
         return $ map fst hasDup 
  else do
    s@UnfolderState{..} <- get 
    -- @  a) Computes the local history of the new event
    -- @NOTE: @CRITICAL OPTIMISE THIS
    prede <- lift $ mapM (\e -> predecessors e evts) history  
    let localHistory = nub $ concat prede ++ history 
        sizeLocalHistory = length localHistory + 1
    --   If we don't need cutoffs, no need to compute the linearization and the new state
    if cutoffs opts 
    then do
      let copyst = GCS.gbst syst
      gstlc <- st_local_conf copyst name localHistory
      if (?.) gstlc
      then error "addEvent: the state of the local configuration is bottom"
      else do
        lift $ showMStr $ "========== FINISHED COMPUTING THE STATE OF LOCAL CONFIG ======"
        isCutoff <- cutoff gstlc sizeLocalHistory
        if isCutoff
        then do 
          lift $ showMStr "add_event: is cutoff!"
          inc_cutoffs
          return []
        else do
          lift $ showMStr $ "add_event: calling add_ev("++show localHistory++")"
          add_ev localHistory evts 
    else do 
      lift $ showMStr $ "add_event: calling add_ev("++show localHistory++")"
      add_ev localHistory evts 
 where
   add_ev localHistory evts = do
     -- Increment the number of events in the prefix
     inc_evs_prefix 
     -- @ 1. Fresh event id 
     neID <- freshCounter
     -- @ 2. Compute the immediate conflicts
     -- @  b) Computes the immediate conflicts of all events in the local configuration
     lhCnfls <- lift $ foldM (\a e -> get_icnf e evts >>= 
                              \es -> return $ es ++ a) [] localHistory >>= return . nub 
     -- @  c) Compute the immediate conflicts
     cnfls <- lift $ compute_conflicts (name,acts) localHistory lhCnfls evts >>= return . nub
     lift $ showMStr $ "add_ev: compute_conflicts result = " ++ show cnfls
     -- @ 3. Insert the new event in the hash table
     let e = Event name acts history [] cnfls [] [] -- gstlc sizeLocalHistory
     lift $ set_event neID e evts 
     -- @ 4. Update all events in the history to include neID as their successor
     lift $ mapM (\e -> add_succ neID e evts) history
     -- @ 5. Update all events in the immediate conflicts to include neID as one 
     lift $ mapM (\e -> add_icnf neID e evts) cnfls 
     inc_evs_per_name name 
     lift $ showMStr $ "add_ev: event_id = " ++ show neID ++ ", history = " ++ show history ++ ", icfn = " ++ show cnfls
     _ <- lift $ getCharDebug
     return $! [neID]

   -- | Compute the global state of the local configuration
   --   by doing a topological sorting
   st_local_conf :: (Domain st act) => st -> EventName -> History -> UnfolderOp st act st
   st_local_conf st ename econf = do
     lift $ showMStr $ "st_local_conf: computing the state of " ++ show econf
     s@UnfolderState{..} <- get
     st' <- st_history st [0] [] econf 
     return $ fst $ drun (widening opts) syst st' ename 

   -- | Actually computes the state of a local configuration
   st_history :: (Domain st act) => st -> EventsID -> EventsID -> History -> UnfolderOp st act st
   st_history st wlist seen hist = 
     case wlist of
       [] -> return st
       (e:es) -> do
         s@UnfolderState{..} <- get
         esucc <- lift $ get_succ e evts
         -- @ update the seen set 
         let seen' = e:seen
         -- @ select the successors which
         -- are part of the history  
             chosen = filter (\e -> e `elem` hist) esucc
         -- @ for each sucessor that is in 
         -- history, check if all immediate predecessors
         -- have been seen, and only then add it to the worklist
         chosen' <- lift $ filterM (\e -> get_pred e evts 
                                >>= \prede -> return $ prede \\ seen' == []) chosen
         let wlist' = es++chosen'
         ename <- lift $ get_name e evts
         let nst = if e == 0 
                   then st
                   else fst $ drun (widening opts) syst st ename
         st_history nst wlist' seen' hist 

-- | Compute conflicts 
-- @NOTE: @CRITICAL: THIS NEEDS TO BE OPTIMISED! 
--  DFS of the unf prefix from bottom stopping when the event:
--  . Is an immediate conflict (or successor) of an event in the local configuration
--  . Is dependent with tr
--  Changed for a worklist
compute_conflicts :: (Show act, Action act) => EventInfo act -> EventsID -> EventsID -> Events act -> IO EventsID
compute_conflicts einfo lh lhCnfls events = do 
  showMStr $ "compute_conflicts: einfo = " ++ show einfo  
  showMStr $ "compute_conflicts: local history = " ++ show lh 
  showMStr $ "compute_conflicts: local history conflicts = " ++ show lhCnfls 
  ev@Event{..} <- get_event "compute_conflicts" botEID events
  compute_conflict [] succ []
  where
    -- compute_conflict receives a list of seen events and the frontier 
    compute_conflict seen [] cfls = return cfls 
    compute_conflict seen (e:rest) cfls = do
      -- check if we have seen this event
      if e `elem` seen
      then compute_conflict seen rest cfls
           -- check if this event is in conflict with any in the local history
      else if e `elem` lhCnfls 
           then compute_conflict (e:seen) rest cfls 
           else do
             ev@Event{..} <- get_event "computeConflict" e events
             -- if e is not in the local history and is dependent with the event
             if not (e `elem` lh) && is_dependent einfo (name,acts) 
             then do
               -- check if there is any immediate conflict in the local configuration
               -- with a predecessor of e, i.e. check if there is a conflict between
               -- e and the new event already  
               lhe <- predecessors e events
               if any (\e -> elem e lhe) (lhCnfls ++ cfls) 
               then compute_conflict (e:seen) rest cfls
               else compute_conflict (e:seen) rest (e:cfls) 
             else compute_conflict (e:seen) (nub $ rest ++ succ) cfls

-- Let e be an event of a configuration C.
--   Let cext be the conflicting extensions of C, 
--    ie. all events e' such that |-e'-| is a subset of C
--        but there is another event ê' such that e' #^ ê'
-- Let ê be the events such that e #^ ê.
-- Can ê intersect cext =!= ê? Yes. There may be immediate conflicts of 
-- e whose roots are not in C.
-- @@ compute potential alternatives @ revised: 08-04-15
possible_altes :: Show act => EventsID -> EventsID -> UnfolderOp st act ()
possible_altes maxevs evs =  do
  s@UnfolderState{..} <- get
  -- @ compute the events of the configuration
  lift $ foldM_ (possible_alte stak evts) S.empty evs where

    -- @ I. V(e) where e is an event that has at least one imm conflict
    possible_alte stack events cext e =  do
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
      -- @ Compute the common parts of the confs that contain e and e': confEvs - (e:succ e) 
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
            -- this check is probably redundant now
            isStackValid = all (\e' -> e' `elem` v) stackE 
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
alt2 :: Show act => EventsID -> EventsID -> UnfolderOp st act (Maybe Alternative)
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
   filter_alte :: Show act => EventsID -> Alternative -> UnfolderOp st act Bool
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
prune_config :: Configuration st -> UnfolderOp st act (Configuration st)
prune_config c@Conf{..} = do
  s@UnfolderState{..} <- get
  nenevs <- lift $ filterEvents enevs evts
  return $ c {enevs = nenevs}

-- | Computes the core of the prefix necessary to continue
--   exploration
-- @NOTE: Optimise this function using Sets.
core :: Show act => EventsID -> EventsID -> Events act -> IO EventsID
core conf d events = do
  let confAndD = conf ++ d
  evs <- mapM (\e -> get_event "compute_core" e events) confAndD
  let altes = concat $ concatMap alte evs 
      core_prefix = confAndD ++ altes
  return $ nub core_prefix 

-- | Prunes the unfolding prefix by potentially the event e and its alternatives
prune :: Show act => EventID -> EventsID -> UnfolderOp st act ()
prune e core = do
  s@UnfolderState{..} <- get
  ev@Event{..} <- lift $ get_event "prune" e evts
  if e `elem` core
  then lift $ reset_alte e evts
  else lift $ del_event e evts
  lift $ mapM_ (\v -> mapM_ (\e -> if e `elem` core then return () else del_event e evts) v) alte
