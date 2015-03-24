{-#LANGUAGE RecordWildCards #-}
module Unfolderless where

import Control.Monad.State.Strict
import Control.Monad.ST.Safe

import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Set (isSubsetOf)
import qualified Data.Vector as V
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.Map as M

import APIStateless
import qualified Model as ML
import qualified Debug.Trace as DT

--import Examples
--import Benchmark

import System.IO.Unsafe
import Prelude hiding (pred)

stateless :: ML.System s -> ML.UIndep -> ST s (UnfolderState s)
stateless sys indep = do
  is <- iState sys indep 
  (a,s) <- runStateT botExplore is 
  return $! s

-- This is the beginning of the exploration
-- where we construct the initial unfolding prefix 
-- with the bottom event
botExplore :: UnfolderOp s () 
botExplore = do 
  iConf <- initialExtensions 
  explore iConf botEID []

-- The extensions from the bottom event
-- After this function, the unfolding prefix denotes
-- the execution of bottom, and contains all extensions from it.
initialExtensions :: UnfolderOp s (Configuration s)
initialExtensions = do
  s@UnfolderState{..} <- get
  let e = botEID
      cevs = [e]
      st = ML.initialState syst
  trs <- lift $ ML.enabledTransitions syst st
  enevs <- V.foldM (\en tr -> expandWith st e cevs tr >>= \es -> return $! (es++en)) [] trs
  s@UnfolderState{..} <- trace ("enabled after e=" ++ show e ++ " are " ++ show enevs) get
  let iConf = Conf st cevs enevs []
  put s{ pcnf = iConf }
  return $! iConf

separator = "-----------------------------------------\n"
-- @@ main function 
explore :: Configuration s -> EventID -> Alternative -> UnfolderOp s ()
explore c@Conf{..} ê alt = do
  is@UnfolderState{..} <- get
  -- @ assert that the global state given by the maximal events is the same as the global state that we have
  -- nst <- lift $ ML.copy stc 
  -- build the global state
  -- emax <- lift $ mapM (\e -> getEvent "explore" e evts) maxevs
  -- gst <- lift $ foldM (\ist e -> ML.modify ist $ fromJust $ lcst e) nst emax 
  -- check <- lift $ ML.equals stc gst
  -- if not check
  -- then error "explore: the states are different"
  -- else do  
  let confEvs = stack -- lift $ getConfEvs maxevs evts
  str <- lift $ showEvents evts
  mtrace (separator ++ "explore(ê = " ++ show ê ++ ", enevs = " ++ show enevs ++ ", alt = " ++ show alt ++ ", stack = " ++ show stack++")\n"++str) $ return () 
  -- @ configuration is maximal?
  if null enevs 
  then do
    -- @ forall special events e in the configuration compute V(e)
    computePotentialAlternatives maxevs cevs 
    return () 
  else do
    -- @ pick an event that is enabled
    e <- if null alt
         then return $ head enevs -- enevs !! (read $ k:[]) -- head enevs
         else do
           let lp = enevs `intersect` alt
           if null lp 
           then do
             evtstr <- lift $ showEvents evts
             stacke <- lift $ mapM (\e -> getEvent "error state" e evts >>= return . snd . evtr) stack
             error $ separator ++ "explore(ê = " ++ show ê ++ ", enevs = " ++ show enevs ++ ", alt = " ++ show alt ++ ", stack = " ++ show stack ++ ")\n"++evtstr++"\nstack_tr="++show stacke
           else return $ head lp   
    -- @ initialize disable of e
    lift $ initializeDisabled evts e ê
    -- @ compute the new enabled events and immediate conflicts after adding *e*
    --   return a new configuration
    nc <- unfold c e
    ms@UnfolderState{..} <- get
    --pevts <- lift $ copyEvents evts 
    -- @ recursive call
    push e
    explore nc e (alt \\ [e])
    pop
    -- @ TODO! Stateless: Garbage collection
    -- @ set the previous disable set
    --setPreviousDisabled pevts
    -- let s' = gc s
    -- @ filter alternatives
    malt <- filterAlternatives maxevs e
    if null malt
    then do
      core <- lift $ computeCore stack evts
      lift $ prune e core evts
      return () 
    else do
      let alt' = head malt
      lift $ addDisabled e ê evts
      -- evs <- lift $ getConfEvs maxevs evts
      -- isConfA <- lift $ isConfiguration evts alt'
      explore c ê (alt' \\ stack)
    --  core <- lift $ computeCore stack evts
    --  lift $ prune e core evts

computeCore :: EventsID -> Events s -> ST s (EventsID)
computeCore conf events = do
  config <- mapM (\e -> predecessors e events >>= return . (e:)) conf
  evs <- mapM (\e -> getEvent "computeCore" e events) conf
  disas <- mapM (\ev -> mapM (\e -> predecessors e events >>= return . (e:)) $ disa ev) evs
  altes <- mapM (\ev -> mapM (\v -> mapM (\e -> predecessors e events >>= return . (e:)) v) $ alte ev) evs
  let evs = config ++ concat disas ++ (concat $ concat altes)
  return $ nub $ concat evs 

prune :: EventID -> EventsID -> Events s -> ST s ()
prune e core events = do
  ev@Event{..} <- getEvent "prune" e events
  mapM_ (\v -> mapM_ (\e -> if e `elem` core then return () else deleteEvent e events) v) alte
  if e `elem` core
  then return ()
  else deleteEvent e events

deleteEvent :: EventID -> Events s -> ST s ()
deleteEvent e events = mtrace ("deleting event " ++ show e) $ do
  ev@Event{..} <- getEvent "deleteEvent" e events
  mapM_ (\e' -> delSuccessor e e' events) pred
  mapM_ (\e' -> delImmCnfl e e' events) icnf 
  H.delete events e

isConfiguration :: Events s -> EventsID -> ST s Bool
isConfiguration evts conf = do
  cnffree <- allM (\e -> getImmediateConflicts e evts >>= \es -> return $! null (es `intersect` conf)) conf
  causaclosed <- causalClosed evts conf conf 
  return $! cnffree && causaclosed

causalClosed :: Events s -> EventsID -> EventsID ->  ST s Bool
causalClosed evts conf [] = return True
causalClosed evts conf (e:es) = do 
  prede <- predecessors e evts
  if all (\e' -> e' `elem` conf) prede
  then causalClosed evts conf es
  else return False

-- We are going to add event e to configuration conf
-- Need to update enable, and immediateConflicts
-- computeExtensions
-- @@ unfold receives the configuration and event that is going to executed
--    and returns the new configuration with that event
--    Build the configuration step by step
unfold :: Configuration s -> EventID -> UnfolderOp s (Configuration s)
unfold conf@Conf{..} e = do
  s@UnfolderState{..} <- get
  tr <- lift $ getEvent "unfold" e evts >>= return . snd . evtr
  -- @ 1. compute the new state after executing the event e
  -- copy the state otherwise it will go wrong 
  copyst <- lift $ ML.copy stc
  -- execute the event e
  (nstc, lcst) <- execute copyst e
  snstc <- lift $ ML.showSigma nstc
  -- update the local state of e
  _ <- mtrace ("unfold(e="++show e++")\nlcst state="++show lcst) $ lift $ setLSigma e lcst evts 
  -- @ 2. compute the new set of maximal events
  iprede <- lift $ getIPred e evts
  let nmaxevs = e:(maxevs \\ iprede)
  -- @ 3. compute the new set of enabled events
  let es = delete e enevs 
  -- - compute the set of events independent with *e*, they will be enabled after *e*
  senevs <- lift $ filterM (\ê -> isIndependent inde e ê evts) es 
  -- - compute the set of enabled transitions at the new state
  entrs <- lift $ ML.enabledTransitions syst nstc 
  -- - filter the enabled transitions that are dependent with h(e); those will be the new events
  --   some of these new events will be conflicting extensions
  netrs <- lift $ V.filterM (\tr -> isDependent_te inde tr e evts) entrs
  nnevs <- V.mapM (expandWith nstc e nmaxevs) netrs >>= return . concat . V.toList 
  -- @ compute all the events of the configuration 
  let confEvs = e:stack -- lift $ getConfEvs nmaxevs evts
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
    
-- expandWith only adds events that have e in the history
-- critical step
expandWith :: ML.Sigma s -> EventID -> EventsID -> (ML.TransitionID, ML.ProcessID) -> UnfolderOp s EventsID
expandWith st e maxevs tr = do
  cst <- lift $ ML.copy st
--  scst <- lift $ ML.showSigma cst
  s@UnfolderState{..} <- trace ("expandWith(e="++show e++",maxevs="++show maxevs++",tr="++show tr++")") $ get 
  -- @ retrieve the immediate successors of e with the same transition id to avoid duplicates
  --   not sure why this is necessary.
  succe <- lift $ getISucc e evts 
           >>= mapM (\e -> getEvent "expandWith" e evts >>= \ev -> return (e,ev)) 
           >>= return . filter (\(e,ev) -> evtr ev == tr)
  -- @ computes h0, the maximal history:
  (ncst, h0) <- computeHistory cst maxevs tr
  -- sncst <- lift $ ML.showSigma ncst
  -- @ check for enabledness
  tr_dis_h0 <- isEnabled evts ncst tr h0 
  if null h0 || (not tr_dis_h0)
  then error $ "expandWith(e="++show e++",tr="++show tr++",h0="++show h0++",tr_dis_h0=" ++ show tr_dis_h0++",maxevs="++show maxevs -- return [] 
  else mtrace ("H0="++show h0++" enabled = " ++ show tr_dis_h0) $ do
    -- e should be a valid maximal event
    if e `elem` h0
    then do
      his <- computeHistories ncst tr e [h0] >>= return . nub 
      mapM (addEvent stack succe tr) his
      mtrace ("other histories="++show his) $ addEvent stack succe tr h0
    else error "e must always be in h0"  

-- @ computeHistory 
--   Input: 
--     - Set of maximal events, 
--     - Transition tr
--   Output: History
computeHistory :: ML.Sigma s -> EventsID -> (ML.TransitionID, ML.ProcessID) -> UnfolderOp s (ML.Sigma s, EventsID)
computeHistory st maxevs tr = do
  s@UnfolderState{..} <- trace ("computeHistory(maxevs="++show maxevs++",tr="++show tr++")") $ get
  -- set of maximal events that are dependent with tr union 
  -- they are events of maxevs that are dependent with tr 
  -- or predecessors of the independent ones that are maximal and dependent
  (maxevs_h0,maxevs') <- lift $ foldM (\(l,r) e -> 
         isDependent_te inde tr e evts >>= \b -> 
            if b then return (e:l,r) else return (l,e:r)) ([],[]) maxevs
  -- @ going up the causality of maxevs' until
  (nst, pmaxevs_h0) <- trace("computeHistory: (maxevs_h0,maxevs') = "++ show (maxevs_h0,maxevs')) $ lift $ pruneConfiguration st inde evts maxevs_h0 tr maxevs'
  let h0 = maxevs_h0 ++ pmaxevs_h0
  trace("result computeHistory(his="++show h0) $ return $! (nst, h0) 

-- @ pruneConfiguration
--   Given a set of maximal events which are independent with transition tr
--   go up in the causality to search for the rest of maximal events that are dependent
pruneConfiguration st inde events pre_his tr es = do
  -- update the state to remove es
  st' <- foldM (\s e -> removeEvent s events e) st es
  -- immd predecessors of es
  predes <- mapM (\e -> getIPred e events) es >>= return . nub . concat
  -- filter the predecessors that are not maximal
  mpredes <- filterM (\e -> successors e events >>= \succe -> return $ null $ succe `intersect` pre_his) predes
  -- split between dependent and independent 
  (es_done,es') <- foldM (\(l,r) e ->
         isDependent_te inde tr e events >>= \b -> 
            if b then return (e:l,r) else return (l,e:r)) ([],[]) mpredes
  if null es'
  then return $! (st', es_done)
  else do
    (nst, es_res) <- pruneConfiguration st' inde events (pre_his ++ es_done) tr es'
    return $! (nst, nub $ es_done ++ es_res) 

-- @ computeNextHistory
--   build a candidate history out of replacing a maximal event e with its immediate predecessors
computeNextHistory :: ML.Sigma s -> EventsID -> (ML.TransitionID, ML.ProcessID) -> EventID -> UnfolderOp s (ML.Sigma s, EventsID)
computeNextHistory st h tr e = mtrace ("computeNextHistory(h="++show h++",tr="++show tr++",e="++show e++")") $ do
  s@UnfolderState{..} <- get
  cst <- lift $ ML.copy st
  -- we want to replace e by its predecessors
  let h' = e `delete` h
  -- predecessors of e
  prede <- lift $ getIPred e evts
  -- filter the predecessors of e that are not maximal
  prede' <- lift $ filterM (\e -> successors e evts >>= \succe -> return $ null $ succe `intersect` h') prede
  -- candidate
  let candidate = h' ++ prede'
  -- new state
  nst <- lift $ removeEvent cst evts e
  computeHistory nst candidate tr  

-- @ computeHistories : worklist algorithm 
--   e must always be a part of the histories
computeHistories :: ML.Sigma s -> (ML.TransitionID, ML.ProcessID) -> EventID -> [EventsID] -> UnfolderOp s [EventsID]
computeHistories _ tr e [] = return []
computeHistories cst tr e (h:hs) = do
  s@UnfolderState{..} <- get
  let h' = e `delete` h
  -- replace one of the maximal events with the predecessors
  -- and prune the configuration
  hs' <- mapM (computeNextHistory cst h tr) h'
  -- filter the configurations where the transition is still enabled
  nhs' <- filterM (\(st,h) -> isEnabled evts st tr h) hs'
  let nhs = map snd nhs'
  -- add nhs to the worklist and recurse
  res <- computeHistories cst tr e $ nub $ nhs ++ hs
  return $! nhs ++ res 
  
-- @ changes the global state with respect to the local state of the event we want to remove 
removeEvent :: ML.Sigma s -> Events s -> EventID -> ST s (ML.Sigma s)
removeEvent s events 0 = return s
removeEvent s events eID = do
  ev@Event{..} <- getEvent "removeEvent" eID events
  prede <- getIPred eID events 
  case lcst of
    Nothing  -> return s
    Just lst -> do
      ss <- ML.showSigma s
      ns <- revertState s events prede lst
      sns <- ML.showSigma ns
      mtrace("removeEvent(eID="++show eID++") previous state="++ss++"\nnew state="++sns) $ return $! ns 

revertState :: ML.Sigma s -> Events s -> EventsID -> ML.LSigma -> ST s (ML.Sigma s)
revertState s events prede [] = return s
revertState s events prede lst =  
  if null prede
  then error $ "revertState: null prede: " ++ show lst
  else do
    es <- mapM (\e -> getEvent "revertState" e events >>= \ev -> return $ (e,fromMaybe [] $ lcst ev)) prede
    -- get the changes
    let (lst',mods) = getChanges lst es
    s' <- ML.modify s mods 
    pprede <- mapM (\e -> getIPred e events) prede >>= return . nub . concat
    revertState s' events pprede lst'

-- returns the effects that need to be found
--         the events that needs to go up
--         the new state
getChanges :: ML.LSigma -> [(EventID, ML.LSigma)] -> (ML.LSigma, ML.LSigma)
getChanges [] prede = ([],[])
getChanges l@((k,v):lst) prede = 
  -- split between the predecessors that modify k
  let (left,right) = partition (\(e,evlst) -> any (\(k',v') -> k == k') evlst) prede 
  in case left of
    [] -> 
      let (lst',mods) = getChanges lst prede
      in ((k,v):lst',mods)
    -- there is an event that modifies k 
    [el@(e,evlst)] -> 
      let l' = M.fromList l
          evlst' = M.fromList evlst
          -- intersection produces the modifications
          mods' =  evlst' `M.intersection` l'
          -- difference of effects with the modifications gives the missing
          rlst = M.toList $ l' `M.difference` mods' 
          (lst',mods) = getChanges rlst (el `delete` prede)
          fmods = M.toList mods' ++ mods
      in (lst', fmods)
    _ -> error "getChanges"

-- @ isEnabled: computes the global state given a set of maximal events and checks if tr is enabled
isEnabled :: Events s -> ML.Sigma s -> (ML.TransitionID, ML.ProcessID) -> EventsID -> UnfolderOp s Bool
isEnabled events st (tr,_) h = do
  s@UnfolderState{..} <- get
  -- cst <- lift $ ML.copy st
  let fn = ML.getTransition syst tr
  es <- lift $  mapM (\e -> getEvent "isEnabled" e events) h 
  -- build the global state
  gst <- lift $ foldM (\ist e -> ML.modify ist $ fromJust $ lcst e) st es
  -- check if the transition is enabled
  checkEn <- lift $ fn gst
  case checkEn of
    Nothing -> return $! False
    _ -> return $! True

-- @ addEvent: Given a transition id and the history
--   adds the correspondent event.
--   *Pre-condition*: The event to be added is not in the unf prefix
--   *Flow*: 
--     1. Generate a fresh event counter: neID
--     2. Compute the set of immediate conflicts
--     3. Insert the new event in the hashtable
--     4. Update all events in the history to include neID as their successor
--     5. Update all events in the immediate conflicts to include neID as one
addEvent :: EventsID -> [(EventID,Event)] -> (ML.TransitionID, ML.ProcessID) -> EventsID -> UnfolderOp s EventsID 
addEvent stack dup tr history = do
  let hasDup = filter (\(e,ev) -> S.fromList (pred ev) == S.fromList history) dup
  if null hasDup  
  then do 
    s@UnfolderState{..} <- get
    -- @ 1. Fresh event id 
    neID <- freshCounter
    -- @ 2. Compute the immediate conflicts
    -- @  a) Computes the local history of the new event
    prede <- mtrace ("addEvent(tr=" ++ show tr++",history="++show history++",neID="++ show neID ++ ")") $ lift $ mapM (\e -> predecessors e evts) history  
    let localHistory = prede `seq` nub $ concat prede ++ history 
    -- @  b) Computes the immediate conflicts of all events in the local configuration
    lhCnfls <- lift $ foldM (\a e -> getImmediateConflicts e evts >>= \es -> return $ es ++ a) [] localHistory >>= return . nub 
    -- @  c) Compute the immediate conflicts
    cnfls <- lift $ computeConflicts inde tr localHistory lhCnfls evts >>= return . nub
    -- @ 3. Insert the new event in the hash table
    let e = Event tr history [] cnfls [] [] Nothing 
    lift $ setEvent neID e evts 
    -- @ 4. Update all events in the history to include neID as their successor
    lift $ mapM (\e -> setSuccessor neID e evts) history
    -- @ 5. Update all events in the immediate conflicts to include neID as one 
    lift $ mapM (\e -> setConflict neID e evts) cnfls 
    return $! [neID]
  else return $! map fst hasDup 

-- @ Compute conflicts  
--  DFS of the unf prefix from bottom stopping when the event:
--  . Is in the local configuration (TODO: this is wrong!!)
--  . Is an immediate conflict (or successor) of an event in the local configuration
--  . Is dependent with tr
--  Changed for a worklist
computeConflicts :: ML.UIndep -> (ML.TransitionID, ML.ProcessID) -> EventsID -> EventsID -> Events s -> ST s EventsID
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
             if not (e `elem` lh) && ML.isDependent uidep tr evtr
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
-- @@ compute potential alternatives 
computePotentialAlternatives :: EventsID -> EventsID -> UnfolderOp s ()
computePotentialAlternatives maxevs evs =  do
  s@UnfolderState{..} <- get
  -- @ compute the events of the configuration
  -- let confEvs = stack -- lift $ getConfEvs maxevs evts
  lift $ foldM_ (computePotentialAlternative stack evts) S.empty evs where

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
      let clfec' = filter (\e -> e `elem` stack) clfe' -- do this check first so that any e' clfe'
      succes' <- mapM (\ce' -> successors ce' events >>= return . (ce':)) clfec'
      let common = stack \\ (concat succes') 
      -- @ 1 and 2
      let v = common ++ [e'] 
          stackE = tail $ dropWhile (/=e) stack -- C(e) is a subset of V
          isStackValid = all (\e' -> e' `elem` v) stackE 
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

-- @ initialize disabled events of *e* based on de(ê)
initializeDisabled :: Events s -> EventID -> EventID -> ST s ()
initializeDisabled events e ê = do -- trace ("init disabled of (e,ê) = " ++ show (e,ê)) $ do
  dê  <- getDisabled ê events
  icê <- getImmediateConflicts ê events
  let de = dê \\ icê
  setDisabled e de events

-- @@ filter alternatives
filterAlternatives :: EventsID -> EventID -> UnfolderOp s Alternatives
filterAlternatives maxevs e =  do 
  s@UnfolderState{..} <- get
  -- @ compute all the events of the configuration
  let confEvs = stack -- <- lift $ getConfEvs maxevs evts
  -- @ compute V(D(e) U e)
  de <- lift $ getDisabled e evts
  vs <- lift $ mapM (\e -> getAlternatives e evts) (e:de) >>= return . concat
  -- @ compute min(dec)
  let ede = e:de 
  edepred <- lift $ mapM (\e -> predecessors e evts) ede >>= return . S.fromList . concat
  dec' <- lift $ filterM (\e -> predecessors e evts >>= \prede -> return $ all (\e -> not $ elem e prede) ede) ede
  let dec = S.fromList dec' 
      maxevset = S.fromList maxevs
  lift $ filterM (filterAlternative evts confEvs maxevset dec) vs where
    -- @@ filter alternative
    filterAlternative events confEvs maxevset dec v =  do
      let vset = S.fromList v
      if maxevset `isSubsetOf` vset
      then do
        cextv <- cex events confEvs v >>= return . S.fromList
        if dec `isSubsetOf` cextv
        then return True 
        else return False 
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
      return $! all (\p -> p `elem` confEvs) eipred

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f [] = return True
allM f (x:xs) = do
  b <- f x
  if b
  then allM f xs
  else return False 