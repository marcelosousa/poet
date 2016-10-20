{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Exploration.SUNF.Unfolder
-- Desc      :  Minimal synchronisation unfolder 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Exploration.SUNF.Unfolder (synfolder) where

import Control.Monad.State.Strict
import Data.Hashable
import Data.List
import Data.Maybe hiding (catMaybes)
import Data.Set (isSubsetOf)
import Domain.Synchron (PEvent(..), System, Act) 
import Exploration.SUNF.API
import Exploration.SUNF.APIStateless
import Exploration.SUNF.APIStid
import Exploration.SUNF.State 
import Haskroid.Hapiroid hiding (Event)
import Prelude hiding (pred)
import System.IO.Unsafe
import Util.Generic
import Util.Printer (unfToDot)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Debug.Trace as T
import qualified Domain.Synchron as SYS 

synfolder :: Bool -> Bool -> System -> IO UnfolderState
synfolder stl cut syst = do
  is    <- i_unf_state stl cut syst 
  (a,s) <- runStateT bot_explore is 
  return $! s

-- This is the beginning of the exploration
-- where we construct the initial unfolding prefix 
-- with the bottom event
bot_explore :: UnfolderOp () 
bot_explore = do 
  iConf <- initial_ext
  explore iConf botEID [] []

-- The extensions from the bottom event
-- After this function, the unfolding prefix denotes
-- the execution of bottom, and contains all extensions from it.
-- v2 (Oct'16): It is no longer the case that it contains all 
-- extensions from bottom. It contains the enabled events of 
-- the underlying PO passed by STID.
initial_ext :: UnfolderOp Configuration
initial_ext = do
  s@UnfolderState{..} <- get
  let e    = botEID
      cevs = [e]
      ist  = SYS.state syst
      nees = SYS.enabled syst ist 
  enevs <- mapM (extend e cevs) nees 
  s@UnfolderState{..} <- get 
  let iConf = Conf ist cevs enevs []
  put s { pcnf = iConf }
  return $! iConf

-- | explore: the main exploration function
--  Input: 
--    1. c: Current configuration
--    2. ê: The latest event added to c. Necessarily a maximal event of c.
--    3. d: The set of disabled events
--    4. alt: Alternative (a corresponding branch in the wake up tree of ODPOR)
explore :: Configuration -> EventID -> EventsID -> Alternative -> UnfolderOp ()
explore c@Conf{..} ê d alt = do
  is@UnfolderState{..} <- get
  -- @ configuration is maximal?
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
                    then error $ sep ++ "A `intersect` en(C) = 0 at explore(ê = " 
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
      Just alt' -> do 
         stid_replay alt' stak 
         explore c ê (e:d) (alt' \\ stak)
    -- @ remove irrelevant parts of the prefix 
    if stateless opts 
    then do
      core_prefix <- lift $ core stak d evts
      prune e core_prefix
    else return ()

-- @@ unfold receives the configuration and event that is going to executed
--    and returns the new configuration with that event
--    Build the configuration step by step
-- @revised 08-04-15
-- @v2 revised 19-10-16
unfold :: Configuration -> EventID -> UnfolderOp Configuration
unfold conf@Conf{..} e = do
  s@UnfolderState{..} <- get
  -- @ 1. compute the new state after executing the event e
  eve <- lift $ get_event "unfold" e evts
  let nstc = SYS.run syst state (name eve)
  -- @ 2. compute the new set of maximal events
  iprede <- lift $ get_pred e evts 
  let nmaxevs = e:(maevs \\ iprede)
  -- @ 3. compute the new set of enabled events
      es = delete e enevs 
      -- - compute the set of enabled pre-events at the new state
      entrs = SYS.enabled syst nstc 
  -- get the events that are still enabled 
  ievs <- lift $ mapM (\e -> get_event "unfold" e evts) es
  -- filter the new enabled pre-events that are not enabled events
  let netrs = filter (\npe -> any (\e@Event{..} -> name /= SYS.pe_name npe) ievs) entrs
  -- compute the new enabled events
  nnevs <- mapM (extend e nmaxevs) netrs 
  -- compute all the events of the configuration 
  let confEvs = e:stak
  -- filter from nnevs events that may have 
   --  immediate conflicts with events in the configuration
  -- @ASSERT The set nnevs should not contain any such event!
  nnevs' <- lift $ filterM (\e -> get_event "unfold" e evts >>= 
                            \ev -> return $ not $ null (icnf ev `intersect` confEvs)) nnevs
  if not $ null nnevs'
  then error "unfold: fatal: invalid configuration"
  else do 
    let nenevs = nnevs ++ es
    -- @ 4. compute the new set of special events
    evs <- lift $ mapM (\e -> get_event "unfold" e evts >>= \ev -> return (e,ev)) confEvs 
    let ncevs = map fst $ filter (\(_,ev) -> not $ null $ icnf ev) evs 
    -- @ build the new configuration
    let nconf = Conf nstc nmaxevs nenevs ncevs
    s@UnfolderState{..} <- get
    put s{ pcnf = nconf }
    return $! nconf

-- | extends the unfolding prefix based on a configuration of a given
--  pre event which contain *e* as an immediate predecessor and returns
--  the event with history h0.
extend :: EventID -> EventsID -> PEvent -> UnfolderOp EventID
extend e maxevs pe = T.trace ("extending the prefix with " ++ show (e,pe)) $ do 
  s@UnfolderState{..} <- get
  -- @ retrieve the immediate successors of e with the same name to avoid duplicates
  succe <- lift $ get_succ e evts 
           >>= mapM (\e -> get_event "extend" e evts >>= \ev -> return (e,ev)) 
           >>= return . filter (\(e,ev) -> name ev == SYS.pe_name pe)
  -- @ computes h0, the maximal history:
  h0 <- history pe maxevs 
  if null h0 
  then error $ "null h0 or c0 at extend(e="++show e
             ++",pe="++show pe++",maxevs="++show maxevs++")"
  else do
    -- e should be a valid maximal event
    -- @NOTE: Remove this check for benchmarking
    if e `elem` h0
    then do
      -- @ Compute other histories, i.e. conflicting extensions 
      his <- if SYS.is_lock pe
             then histories_lock pe h0
             else return []
      mapM (add_event stak succe pe) his
      add_event stak succe pe (h0,[])
    else error "e must always be in h0"  

-- | history computes the largest history of an event which we call h0 
--   Input: 
--     - Pre-event 
--     - Set of maximal events, 
--   Output: History
-- An history is composed of events of maxevs that are dependent with pe 
-- or dependent predecessors of the independent ones that are maximal
-- @TODO: Oct 2016. Modify this version
-- Only two types of events can have two immediate predecessors:
--  LOCK and JOIN.
-- For the remainder, we only need to get the latest event in the process
-- and that will correspond to h0.
history :: PEvent -> EventsID -> UnfolderOp History
history pe@PEv{..} maxevs = do
  let name = SYS.pe_name pe
  e_proc <- latest_ev_proc name maxevs
  case act_ty act of 
    JOIN -> do 
      let exit_tid = SYS.pe_exit_tid act
      e_exit <- exit_ev exit_tid maxevs
      compute_hist e_proc e_exit 
    LOCK -> do
      let mut_addr = SYS.pe_mut_addr act
      e_unlk <- latest_ev_unlk mut_addr maxevs
      compute_hist e_proc e_unlk 
    _ -> return [e_proc]

-- | Going to compute the histories of a lock event.
--   This history can be composed of at most two events
--   one to enable this transition in the thread and 
--   an unlock from another thread.
histories_lock :: PEvent -> EventsID -> UnfolderOp [(History,EventsID)]
histories_lock pe h0 = do
  let name     = SYS.pe_name pe
      mut_addr = SYS.pe_mut_addr pe
  -- 1. Get e_proc: the last event in the thread of pe
  e_proc <- latest_ev_proc name h0
  -- 2. Get e_unlk: the event in hs that is the unlock
  let hs_unlk = filter (SYS.is_unlock_of mut_addr) h0
  case hs_unlk of
    -- @TODO: check that h0 = [e_unlk] or [e_unlk, e_proc] ?
    [e_unlk] -> do 
      e_unlk' <- find_prev_unlock e_unlk
      histories_lock_inner pe e_proc e_unlk' 
    _ -> error "histories_lock: there is no unlock event in h0"

-- | check if {e_proc, e_unlk} can be an history of 
--   pe (accounting for the fact that e_proc can be
--   a predecessor of e_unlk.
--   If that is the case, return {e_proc, e_unlk}, e_lk
--   where e_lk is the lock event that is an immediate
--   successor of e_unlk, with the same name as pe
histories_lock_inner :: PEvent -> EventID -> EventID -> UnfolderOp [(History,EventsID)]
histories_lock_inner pe e_proc e_unlk = do
  c1 <- is_same_or_succ e_proc e_unlk 
  if c1 
  then return []
  else do
      e_lk    <- find_lock_cnfl pe e_unlk
      c2      <- is_concur e_proc e_unlk
      let h   = if c2 then [e_proc, e_unlk] else [e_unlk]
      e_unlk' <- find_prev_unlock e_unlk
      hist    <- histories_lock_inner pe e_proc e_unlk'
      return $ (h,[e_lk]):hist

-- @ add_event: Given a pre-event and the history
--   adds the correspondent event.
--   *Note*: We may try to add an even that is already in the prefix
--   *Flow*: 
--     1. Generate a fresh event counter: neID
--     2. Compute the set of immediate conflicts
--     3. Insert the new event in the hashtable
--     4. Update all events in the history to include neID as their successor
--     5. Update all events in the immediate conflicts to include neID as one
add_event :: EventsID -> [(EventID, Event)] -> PEvent -> (History, EventsID) -> UnfolderOp EventID 
add_event stack dup pe (history,cnfls) = do
  let hasDup = filter (\(e,ev) -> S.fromList (pred ev) == S.fromList history) dup
  if not $ null hasDup  
  then if length hasDup > 1
       then error "the number of duplicates is higher than 1"
       else return $ fst $ head hasDup 
  else do 
    -- Increment the number of events in the prefix
    inc_evs_prefix 
    -- @ 1. Fresh event id 
    neID <- freshCounter
    -- @ 2. Compute the immediate conflicts
    -- @  b) Computes the immediate conflicts of all events in the local configuration
    -- @ 3. Insert the new event in the hash table
    let name = SYS.pe_name pe
        acts = SYS.pe_act pe
        e = Event name acts history [] cnfls [] []
    s@UnfolderState{..} <- get
    lift $ set_event neID e evts 
    -- @ 4. Update all events in the history to include neID as their successor
    lift $ mapM (\e -> add_succ neID e evts) history
    -- @ 5. Update all events in the immediate conflicts to include neID as one 
    lift $ mapM (\e -> add_icnf neID e evts) cnfls 
    inc_evs_per_name name 
    return $! neID

-- Let e be an event of a configuration C.
--   Let cext be the conflicting extensions of C, 
--    ie. all events e' such that |-e'-| is a subset of C
--        but there is another event ê' such that e' #^ ê'
-- Let ê be the events such that e #^ ê.
-- Can ê intersect cext =!= ê? Yes. There may be immediate conflicts of 
-- e whose roots are not in C.
-- @@ compute potential alternatives @ revised: 08-04-15
possible_altes :: EventsID -> EventsID -> UnfolderOp ()
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
      -- @ Compute the common parts of the configurations 
      --   that contain e and e': confEvs - (e:succ e) 
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
alt2 :: EventsID -> EventsID -> UnfolderOp (Maybe Alternative)
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
   filter_alte :: EventsID -> Alternative -> UnfolderOp Bool
   filter_alte d v = do
     s@UnfolderState{..} <- get
     cnfs <- lift $ mapM (\e -> get_icnf e evts) v >>= return . nub . concat
     let isConf = not $ any (\e -> e `elem` stak) cnfs
         isJust = all (\e -> e `elem` cnfs) d
     return $ isJust && isConf 

