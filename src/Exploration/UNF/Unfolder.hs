{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE FlexibleContexts #-}
module Exploration.UNF.Unfolder (unfolder) where

-- import Util.Printer (unfToDot)
import Control.Monad.State.Strict
import Data.List
import Data.Maybe hiding (catMaybes)
import Data.Set (isSubsetOf)
import Exploration.UNF.API
import Exploration.UNF.Cutoff.McMillan
import Exploration.UNF.State
import Language.SimpleC.Util
import Prelude hiding (pred)
import Util.Generic
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Model.GCS as GCS

unfolder :: GCS.Collapsible st a => Bool -> Bool -> GCS.System st a -> IO (UnfolderState st a)
unfolder stl cut syst = do 
  putStrLn $ "unfolder start:\n" ++ show_symt (GCS.symt syst) 
  is    <- i_unf_state stl cut syst 
  (a,s) <- runStateT bot_explore is 
  return $! s

-- This is the beginning of the exploration
-- where we construct the initial unfolding prefix 
-- with the bottom event
bot_explore :: GCS.Collapsible st act => UnfolderOp st act () 
bot_explore = do 
  iConf <- initial_ext
  explore iConf botEID [] []

-- The extensions from the bottom event
-- After this function, the unfolding prefix denotes
-- the execution of bottom, and contains all extensions from it.
initial_ext :: GCS.Collapsible st act => UnfolderOp st act (Configuration st)
initial_ext = do
  s@UnfolderState{..} <- get
  let e = botEID
      cevs = [e]
      st = GCS.gbst syst
      trs = GCS.enabled syst st
  lift $ putStrLn ("initial_ext: enabled threads " ++ show trs)
  enevs <- foldM (\en tr -> extend e cevs st tr >>= \es -> return $! (es++en)) [] trs
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
explore :: GCS.Collapsible st act => Configuration st -> EventID -> EventsID -> Alternative -> UnfolderOp st act ()
explore c@Conf{..} ê d alt = do
  is@UnfolderState{..} <- get
  str <- lift $ showEvents evts
  lift $ putStrLn (separator ++ "explore(ê = " ++ show ê ++ ", d = " ++ show d 
       ++ ", enevs = " ++ show enevs ++ ", alt = " 
       ++ show alt ++ ", stack = " ++ show stak
       ++")\nState:\n"++show state++"\nEvents in the Prefix\n"++str++"\n"++separator) 
  k <- lift $ getChar
  -- @ configuration is maximal?
  -- if null enevs 
  if null enevs 
  then do
    -- @ forall events e in Conf with immediate conflicts compute V(e)
    --   and check if its a valid alternative
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
unfold :: GCS.Collapsible st act => Configuration st -> EventID -> UnfolderOp st act (Configuration st)
unfold conf@Conf{..} e = do
  s@UnfolderState{..} <- get
  -- @ 1. compute the new state after executing the event e
  nstc <- execute state e 
  -- @ 2. compute the new set of maximal events
  iprede <- lift $ get_pred e evts 
  let nmaxevs = e:(maevs \\ iprede)
  -- @ 3. compute the new set of enabled events
      es = delete e enevs 
  -- - compute the set of events independent with *e*, they will be enabled after *e*
  senevs <- lift $ filterM (\ê -> is_independent e ê evts) es
  -- - get the name of these independent events
  ievs <- lift $ mapM (\e -> get_event "unfold" e evts) senevs 
  -- - compute the set of enabled threads at the new state
  let entrs = GCS.enabled syst nstc 
  -- - filter the enabled threads that are dependent with h(e); those will be the new events
  --   some of these new events will be conflicting extensions
  --   this is computed by checking that the enabled thread does not 
  --   contain independent events with the new event. 
  --   @TODO SOUDNESS PROOF!
  evTId <- lift $ get_tid e evts
  let netrs = filter (\t -> all (\e@Event{..} -> evTId /= t) ievs) entrs
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
   execute :: GCS.Collapsible st act => st -> EventID -> UnfolderOp st act st 
   execute st e = do
     s@UnfolderState{..} <- get
     ev@Event{..} <- lift $ get_event "execute" e evts
     let (nst,nacts) = GCS.dcollapse syst st name
         nev = ev {acts = nacts}
     lift $ set_event e nev evts
     return nst 

-- | extend adds new events to the unfolding prefix where the event *e* is 
--   an immediate predecessor.
-- expandWith returns per thread/transition, the event with the largest history
-- and necessarily an extension of the current configuration.
extend :: GCS.Collapsible st act => EventID -> EventsID -> st -> GCS.TId -> UnfolderOp st act EventsID
extend e maxevs st th = do
  lift $ putStrLn ("extend: e = " ++ show e ++ ", maxevs = " ++ show maxevs ++ ", th = " ++ show th)
  s@UnfolderState{..} <- get
  -- @ call the execution engine (e.g. collapse) to retrieve
  --  i. the name of the new events
  --  ii. the set of actions performed that enables us to construct
  --      a global state (i.e. the state of the configuration) and 
  --      perform sound independence/interference reasoning
  let new_events = GCS.collapse False syst st th -- :: [(st,pos,[act])]
  lift $ putStrLn ("extend: collapse result\n" ++ show new_events)
  -- @ For each triple (new_state,pos,acts) given by execution engine, 
  --   generate events with that name and actions. 
  nevs <- mapM (\(nst,pos,acts) -> ext e maxevs (th,pos) acts) new_events
  return $ concat nevs

-- | extends the unfolding prefix based on a configuration (denoted with the 
-- set of maximal events with events) of a given name which contain
-- *e* as an immediate predecessor and returns the event with history h0.
ext :: (Show act, GCS.Collapsible st act) => EventID -> EventsID -> EventName -> act -> UnfolderOp st act EventsID
ext e maxevs êname êacts = do 
  lift $ putStrLn $ "extending the prefix " ++ show (e,êname,êacts)  
  s@UnfolderState{..} <- get
  -- @ retrieve the immediate successors of e with the same name to avoid duplicates
  succe <- lift $ get_succ e evts 
           >>= mapM (\e -> get_event "expandWith" e evts >>= \ev -> return (e,ev)) 
           >>= return . filter (\(e,ev) -> name ev == êname)
  -- @ computes h0, the maximal history:
  h0 <- history (êname,êacts) maxevs 
  if null h0
  then error $ "null h0 at expandWith(e="++show e
             ++",name="++show êname++",maxevs="++show maxevs++")"
  else do
    -- e should be a valid maximal event
    -- @NOTE: Remove this check for benchmarking
    if e `elem` h0
    then do
      -- @ Compute other histories
      his <- if GCS.isBlocking êacts 
             then do
               -- @ Compute histories related to locks
               prede <- lift $ predecessors e evts 
               histories_lock (êname,êacts) e prede (e `delete` h0)
             else histories (êname,êacts) e [h0] [h0] 
      mapM (add_event stak succe êname êacts) his
      add_event stak succe êname êacts h0
    else error "e must always be in h0"  

-- | history computes the largest history of an event which we call h0 
--   Input: 
--     - Metainformation about the event (name, actions) 
--     - Set of maximal events, 
--   Output: History
-- An history is composed of events of maxevs that are dependent with (name,acts) 
-- or dependent predecessors of the independent ones that are maximal
history :: (GCS.Collapsible st act) => EventInfo act -> EventsID -> UnfolderOp st act History
history einfo maxevs = do
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
      mpredes <- filterM (\e -> successors e events >>= 
                          \succe -> return $ null $ succe `intersect` (pre_his ++ predes)) predes
      -- split between dependent and independent 
      (pre_his',es') <- partition_dependent einfo events (pre_his,[]) mpredes
      if null es'
      then return pre_his' 
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
histories :: (GCS.Collapsible st act) => EventInfo act -> EventID -> Histories -> Histories -> UnfolderOp st act Histories
histories einfo@(ename,_) e wlist hists =
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
      hs' <- mapM (next_history h) hc
      let chs = nub hs' -- remove the repeated in the new histories
          wlist' = filter (\h -> not $ h `elem` hs) chs -- filter the ones in the worklist
          nwlist = wlist' ++ hs
          nhists = filter (\h -> not $ h `elem` hists) chs -- filter the ones in the other set 
      histories einfo e nwlist nhists 
 where
   -- @ next_history 
   --   build a candidate history out of replacing a maximal
   --   event e' with its immediate predecessors
   next_history h e' = do
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
     history einfo (h'++prede')

-- | Going to compute the histories of a lock event.
--   Even though we define a blocking event to be both
--   a lock and a unlock event, since an unlock event
--   always has one immediate predecessor, by removing it 
--   histories_lock will always be called with
--   hs = []. 
--   This history can be composed of at most two events
--   one to enable this transition in the thread and 
--   an unlock from another thread.
--   If the event e is in the thread, then we know
--   that e' must be a blocking event. 
--   If the event e is not in the thread, then we
--   know that it is a blocking event and so there
--   is no other history. 
histories_lock :: (GCS.Collapsible st act) => EventInfo act -> EventID -> EventsID -> EventsID -> UnfolderOp st act Histories
histories_lock info@(ne_name,ne_acts) e preds_e hs = 
  case hs of
    [] -> return []
    [e'] -> do 
      -- @ Necessarily a lock event (see function header comment)
      s@UnfolderState{..} <- get
      ev <- lift $ get_event "histories_lock" e evts
      if fst (name ev) == fst ne_name 
      then do
        -- @ proc(e) == proc(new_event)
        --   Hence e' must be a blocking event, namely an unlock
        ev' <- lift $ get_event "histories_lock" e' evts
        let acte' = acts ev'
        -- @ Check that indeed e' is an unlock.
        -- @NOTE: Remove this check in production
        if GCS.isUnlockOf ne_acts acte' 
        then do
          me'' <- prev_unlock info preds_e e'
          case me'' of
            -- @ [e] is not a valid history 
            Nothing -> return []
            -- @ [e] is a valid history
            Just [] -> return [[e]]
            -- @ [e,e''] is a valid history
            --   and by construction e'' is unlock
            Just [e''] -> do
              let hN = [e,e'']
              rest <- histories_lock info e preds_e [e'']
              return $ hN:rest
        else error $ "histories_lock: can't happen! " ++ show ne_acts ++ " " ++ show acte'
      else return [] -- @ proc(e) != tr
    _ -> error $ "histories_lock fatal :" ++ show (info,e,preds_e,hs)
 where
   -- | finds the previous unlock in the causality chain of
   --   an event that was an unlock.
   --   Input:
   --   einfo: meta information about the new event
   --   preds_e: set of predecessors of the *event e*
   --   e': the unlock event that we are going to remove
   --       and find the previous in the causality chain
   --   Returns the previous unlock it exists
   prev_unlock einfo@(ename,eacts) preds_e e' = do
     s@UnfolderState{..} <- get
     iprede' <- lift $ get_pred e' evts
     (es_done,es) <- lift $ partition_dependent einfo evts ([],[]) iprede' 
     -- @ es_done is either empty or has one event
     case es_done of
       -- @ none of the immediate predecessors 
       --   is dependent with the actions of the new event 
       [] -> do
         lres <- mapM (prev_unlock einfo preds_e) es >>= return . catMaybes
         case lres of
           Nothing -> return Nothing
           Just [] -> return $ Just []
           Just [x] -> return $ Just x
           _ -> error "findPrevUnlock: not sure what happens here!"
       [e] -> do
         -- @ one of the immediate predecessors 
         --   is a lock or an unlock 
         ev@Event{..} <- lift $ get_event "findPrevUnlock" e evts
         -- @ NOTE: Optimise this check; no need to traverse twice
         if GCS.isLockOf eacts acts
         -- @ it is a lock, check if it maximal
         then if e `elem` preds_e                 
              then return Nothing                 
              else prev_unlock einfo preds_e e
         -- @ it not a lock, must be an unlock?
         --   check if it is maximal (found it if maximal!) 
         else if GCS.isUnlockOf eacts acts
              then if e `elem` preds_e 
                   then return $ Just []
                   else return $ Just [e]
              else do
                -- @ ASSERT: IS THIS CODE EVERY REACHABLE?
                error "prev_unlock: an interferring event with a lock is not a lock or unlock"
                -- let iacts = GCS.initialActs syst
                -- if any (\a -> (GCS.Lock (GCS.varOf a)) `elem` iacts) act
                -- then return Nothing
                -- else return $ Just []
       _ -> error "prev_unlock: two events are dependent and that can't happen"

-- @ add_event: Given a transition id and the history
--   adds the correspondent event.
--   *Note*: We may try to add an even that is already in the prefix
--   *Flow*: 
--     1. Generate a fresh event counter: neID
--     2. Compute the set of immediate conflicts
--     3. Insert the new event in the hashtable
--     4. Update all events in the history to include neID as their successor
--     5. Update all events in the immediate conflicts to include neID as one
add_event :: (GCS.Collapsible st act) => EventsID -> [(EventID,Event act)] -> EventName -> act -> History -> UnfolderOp st act EventsID 
add_event stack dup name acts history = do
  let hasDup = filter (\(e,ev) -> S.fromList (pred ev) == S.fromList history) dup
  if not $ null hasDup  
  then if length hasDup > 1
       then error "add_event: the number of duplicates is higher than 1"
       else return $ map fst hasDup 
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
      if GCS.isBottom gstlc
      then error "addEvent: the state of the local configuration is bottom"
      else do
        isCutoff <- cutoff gstlc sizeLocalHistory
        if isCutoff
        then do 
          inc_cutoffs
          return []
        else add_ev localHistory evts 
    else add_ev localHistory evts 
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
     -- @ 3. Insert the new event in the hash table
     let e = Event name acts history [] cnfls [] [] -- gstlc sizeLocalHistory
     lift $ set_event neID e evts 
     -- @ 4. Update all events in the history to include neID as their successor
     lift $ mapM (\e -> add_succ neID e evts) history
     -- @ 5. Update all events in the immediate conflicts to include neID as one 
     lift $ mapM (\e -> add_icnf neID e evts) cnfls 
     inc_evs_per_name name 
     return $! [neID]

   -- | Compute the global state of the local configuration
   --   by doing a topological sorting
   st_local_conf ::(GCS.Collapsible st act) => st -> EventName -> History -> UnfolderOp st act st
   st_local_conf st ename econf = do
     s@UnfolderState{..} <- get
     st' <- st_history st [0] [] econf 
     return $ GCS.simple_run syst st' ename 

   -- | Actually computes the state of a local configuration
   st_history :: (GCS.Collapsible st act) => st -> EventsID -> EventsID -> History -> UnfolderOp st act st
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
                   else GCS.simple_run syst st ename
         st_history nst wlist' seen' hist 

-- | Compute conflicts 
-- @NOTE: @CRITICAL: THIS NEEDS TO BE OPTIMISED! 
--  DFS of the unf prefix from bottom stopping when the event:
--  . Is an immediate conflict (or successor) of an event in the local configuration
--  . Is dependent with tr
--  Changed for a worklist
compute_conflicts :: (Show act, GCS.Action act) => EventInfo act -> EventsID -> EventsID -> Events act -> IO EventsID
compute_conflicts einfo lh lhCnfls events = do 
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
             -- if e is not in the local history and his dependent with the event
             if not (e `elem` lh) && is_dependent einfo (name,acts) 
             then do
               -- check if there is any immediate conflict in the local configuration
               -- with a predecessor of e, i.e. check if there is a conflict between
               -- e and the new event already  
               lhe <- predecessors e events
               if any (\e -> elem e lhe) lhCnfls 
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
