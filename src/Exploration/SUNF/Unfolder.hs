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
import qualified Data.HashTable.IO as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Debug.Trace as T
import qualified Domain.Synchron as SYS 

synfolder :: Bool -> Bool -> System -> IO UnfolderState
synfolder stl cut syst = do
  putStrLn "synfolder"
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
  lift $ putStrLn "initial_ext"
  s@UnfolderState{..} <- get
  let e    = botEID
      cevs = [e]
      mevs = (M.fromList [(-1,botEID)])
      ist  = SYS.state syst
      nees = SYS.enabled syst ist 
  lift $ putStrLn $ "initial enabled threads " ++ show nees
  enevs <- mapM (extend e cevs mevs) nees 
  s@UnfolderState{..} <- get 
  let iConf = Conf ist cevs mevs enevs [] 
  put s { pcnf = iConf }
  return $! iConf

separator = "-----------------------------------------\n"

-- | explore: the main exploration function
--  Input: 
--    1. c: Current configuration
--    2. ê: The latest event added to c. Necessarily a maximal event of c.
--    3. d: The set of disabled events
--    4. alt: Alternative (a corresponding branch in the wake up tree of ODPOR)
explore :: Configuration -> EventID -> EventsID -> Alternative -> UnfolderOp ()
explore c@Conf{..} ê d alt = do
  is@UnfolderState{..} <- get
  str <- lift $ showEvents evts
  lift $ putStrLn (separator ++ "explore(ê = " ++ show ê ++ ", d = " ++ show d 
       ++ ", enevs = " ++ show enevs ++ ", alt = " 
       ++ show alt ++ ", stack = " ++ show stak++")\nState:\n"++SYS.show_st state++str++separator) 
  k <- lift $ getChar
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
         lift $ print $ "Computed alt': " ++ show alt' 
         stid_replay $ tail $ reverse alt'
         let c' = remove_ev c e
         explore c' ê (e:d) (alt' \\ stak)
    -- @ remove irrelevant parts of the prefix 
    if stateless opts 
    then do
      core_prefix <- lift $ core stak d evts
      prune e core_prefix
    else return ()

remove_ev :: Configuration -> EventID -> Configuration
remove_ev conf@Conf{..} e = 
  let en = delete e enevs
  in conf {enevs = en}

-- @@ unfold receives the configuration and event that is going to executed
--    and returns the new configuration with that event
--    Build the configuration step by step
-- @revised 08-04-15
-- @v2 revised 19-10-16
unfold :: Configuration -> EventID -> UnfolderOp Configuration
unfold conf@Conf{..} e = do
  lift $ putStrLn $ "unfold: event *" ++ show e ++ "* with state\n" ++ SYS.show_st state
  s@UnfolderState{..} <- get
  -- @ 1. compute the new state after executing the event e
  eve <- lift $ get_event "unfold" e evts
  lift $ putStrLn $ "unfold: run" 
  let nstc = SYS.run syst state (name eve)
  -- @ 2. compute the new set of maximal events
  iprede <- lift $ get_pred e evts 
  let nmaxevs = e:(maevs \\ iprede)
  -- @ 3. update the maximal event per process
      nmaxevsproc = M.insert (fromInteger $ fst $ name eve) e maxev  
      -- - compute the set of enabled pre-events at the new state
      entrs = SYS.enabled syst nstc 
  -- @ 4. compute the new set of enabled events
  es <- filterM (clean entrs) $ delete e enevs
  -- get the events that are still enabled 
  ievs <- lift $ mapM (\e -> get_event "unfold" e evts) es
  -- filter the new enabled pre-events that are not enabled events
  let netrs = filter (\npe -> all (\e@Event{..} -> name /= SYS.pe_name npe) ievs) entrs
  -- compute the new enabled events
  lift $ putStrLn $ "unfold: new state\n" ++ SYS.show_st nstc
  lift $ putStrLn $ "unfold: enabled pre-events:\n" ++ (foldr (\a b -> show a ++ "\n" ++ b) "" netrs)
  nnevs <- mapM (extend e nmaxevs nmaxevsproc) netrs 
  -- compute all the events of the configuration 
  let confEvs = e:stak
      nenevs = nnevs ++ es
  -- @ 4. compute the new set of special events
  evs <- lift $ mapM (\e -> get_event "unfold" e evts >>= 
                      \ev -> return (e,ev)) confEvs 
  let ncevs = map fst $ filter (\(_,ev) -> not $ null $ icnf ev) evs
  -- @ 5. if this event is an unlock, we add it 
  -- @ build the new configuration
  let nconf = Conf nstc nmaxevs nmaxevsproc nenevs ncevs
  s@UnfolderState{..} <- get
  put s{ pcnf = nconf }
  return $! nconf

clean :: [PEvent] -> EventID -> UnfolderOp Bool
clean ts e = do
  s@UnfolderState{..} <- get
  ev@Event{..} <- lift $ get_event "clean" e evts
  return $ any (\t -> tid t == fst name) ts

-- | extends the unfolding prefix based on a configuration of a given
--  pre event which contain *e* as an immediate predecessor and returns
--  the event with history h0.
extend :: EventID -> EventsID -> MEventsID -> PEvent -> UnfolderOp EventID
extend e maxevs mpevs pe = do
  lift $ putStrLn $ "extend: e=" ++ show e ++ ", maxevs: " 
                 ++ show maxevs ++ ", maxevs_proc: " ++ show mpevs
                 ++ ", pe: " ++ show pe 
  s@UnfolderState{..} <- get
  if SYS.is_lock pe
  then do
    -- @ compute the addr in question
    let addr = SYS.pe_mut_addr $ act pe 
    -- @ compute the reversed sequence of unlocks
    unlks_addr <- unlocks_of_addr (tid pe) addr (e:stak)
    -- @ compute the predecessor in the thread
    let name = SYS.pe_name pe
    e_proc <- latest_ev_proc (fst name) mpevs
    case unlks_addr of
      -- @ first lock ever
      [] -> do 
        l <- add_event stak pe ([e_proc],[])
        case l of 
          [e'] -> return e'
          _ -> error "extend: add_event returned invalid list"
      -- @ more than one lock 
      (u:r) -> do
        h0  <- compute_hist e_proc u 
        his <- histories_lock pe e_proc r 
        lift $ putStrLn $ "extend: lock histories " ++ show his
        mapM (add_event stak pe) his
        l <- add_event stak pe (h0,[])
        case l of 
          [e'] -> return e'
          _ -> error "extend: add_event returned invalid list"
  else do
    -- @ computes h0, the maximal history:
    h0 <- history pe maxevs mpevs
    if null h0 
    then error $ "null h0 or c0 at extend(e="++show e
               ++",pe="++show pe++",maxevs="++show maxevs
               ++",maxprocevs="++show mpevs++")"
    else add_event' stak pe (h0,[])

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
history :: PEvent -> EventsID -> MEventsID -> UnfolderOp History
history pe@PEv{..} maxevs mpevs = do
  let name = SYS.pe_name pe
  e_proc <- latest_ev_proc (fst name) mpevs 
  case act_ty act of 
    JOIN -> do 
      let exit_tid = SYS.pe_exit_tid act
      e_exit <- latest_ev_proc exit_tid mpevs 
      compute_hist e_proc e_exit 
    LOCK -> error "should never happen"
    _ -> return [e_proc]

-- | Going to compute the histories of a lock event.
--   This history can be composed of at most two events
--   one to enable this transition in the thread and 
--   an unlock from another thread.
-- | check if {e_proc, e_unlk} can be an history of 
--   pe (accounting for the fact that e_proc can be
--   a predecessor of e_unlk.
--   If that is the case, return {e_proc, e_unlk}, e_lk
--   where e_lk is the lock event that is an immediate
--   successor of e_unlk, with the same name as pe
histories_lock :: PEvent -> EventID -> EventsID -> UnfolderOp [(History,EventsID)]
histories_lock pe e_proc []      = error "histories_lock: empty list error" 
histories_lock pe e_proc [lk]    = return [([e_proc],[lk])]
histories_lock pe e_proc (e_lk:e_unlk:r) = do
  c1 <- is_same_or_succ e_proc e_unlk 
  if c1 
  then return []
  else do
      -- let e_lk = head r -- find_lock_cnfl pe e_unlk
      c2      <- is_pred e_proc e_unlk
      let h   = if c2 then [e_proc, e_unlk] else [e_unlk]
      hist    <- histories_lock pe e_proc $ tail r
      return $ (h,[e_lk]):hist

is_duplicate :: PEvent -> History -> (EventID, Event) -> Bool
is_duplicate pe@PEv{..} hist (_,e@Event{..}) =
  name == (tid, idx) && (S.fromList hist) == (S.fromList pred) 

-- @ add_event: Given a pre-event and the history
--   adds the correspondent event.
--   *Note*: We may try to add an even that is already in the prefix
--   *Flow*: 
--     1. Generate a fresh event counter: neID
--     2. Compute the set of immediate conflicts
--     3. Insert the new event in the hashtable
--     4. Update all events in the history to include neID as their successor
--     5. Update all events in the immediate conflicts to include neID as one
add_event :: EventsID -> PEvent -> (History, EventsID) -> UnfolderOp EventsID 
add_event stack pe (history,cnfls) = do
  s@UnfolderState{..} <- get
  m <- lift $ H.toList evts
  case filter (is_duplicate pe history) m of
    [] -> do
      e <- add_event' stack pe (history, cnfls)
      return [e]
    [(eID,_)] -> do
      lift $ putStrLn $ "add_event duplicate! " ++ show pe ++ ", hist = " ++ show history 
      return [eID]
    _ -> error "add_event: more than one event with the same history"  

add_event' :: EventsID -> PEvent -> (History, EventsID) -> UnfolderOp EventID 
add_event' stack pe (history, cnfls) = do
  lift $ putStrLn $ "add_event': " ++ show pe ++ ", hist = " ++ show history ++ ", cnfl = " ++ show cnfls
  -- Increment the number of events in the prefix
  inc_evs_prefix 
  -- @ 1. Fresh event id 
  neID <- freshCounter
  -- @ 2. Compute the immediate conflicts
  -- @  b) Computes the immediate conflicts of all events in the local configuration
  -- @ 3. Insert the new event in the hash table
  let name = SYS.pe_name pe
      acts = SYS.pe_act pe
      e = Event name history [] cnfls [] [] acts
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
            v = e':common 
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

