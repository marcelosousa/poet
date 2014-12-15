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

import qualified Debug.Trace as T
import System.IO.Unsafe

trace a b = b -- T.trace
ptrace= T.trace

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
  iConf <- initialExtensions botEID 
  explore iConf botEID []

-- The extensions from the the bottom event
-- After this function, the unfolding prefix denotes
-- the execution of bottom, and contains all extensions from it.
initialExtensions :: EventID -> UnfolderOp s (Configuration s)
initialExtensions e = do
  s@UnfolderState{..} <- get
  let nconfes = [e]
      st = ML.initialState syst
  trs <- lift $ ML.enabledTransitions syst st
  enabledE <- V.foldM (\en tr -> addEvents [] 0 nconfes tr >>= \es -> return (es++en)) [] trs
  let iConf = Conf st nconfes enabledE []
  put s{ pcnf = iConf }
  return iConf

--  
explore :: Configuration s -> EventID -> Alternative -> UnfolderOp s ()
explore c@Conf{..} ê alt = do
  s <- get
  -- let prevConf = configurations s
  --(en,cext) <- extensions conf     -- compute the extensions of a configuration  
  --config@Configuration{..} <- getConfiguration conf
  -- @ configuration is maximal?
  if null enevs 
  then do
    -- @ forall special events e in the configuration compute V(e)
    computePotentialAlternatives maxevs cevs 
    return () 
  else undefined

{-
  then do 
  else trace ("configuration id=" ++ show conf ++ " is not maximal: " ++ show cevs) $ do
    let e = if null alt
            then head en
            else head $ en `intersect` alt
    ptrace ("picked enabled event id=" ++ show e) $ computeDisable e ê
    addV [] e
    nconf <- computeEnabled (en \\ [e]) config e  -- compute new enabled events to the configuration nconf and the immediate conflicts
    sb <- get
    let oldDisable = disable sb
    explore nconf e (alt \\ [e])
    -- TODO: Reset the state to make it stateless
    s@UnfolderState{..} <- get
    -- let s' = gc s
    let ns = s{ disable = oldDisable, configurations = oldConf } -- not sure why this makes sense
    put ns
    malt <- computeAlternatives config e
    if null malt
    then ptrace ("finishing because " ++ show e ++ " has no alternatives") $ return () 
    else do 
      let alt' = head malt
      addDisable ê e
      explore conf ê (alt' \\ cevs)
-}

{-
-- We are going to add event e to configuration conf
-- Need to update enable, and immediateConflicts
-- computeExtensions
computeEnabled :: EventsID -> Configuration -> EventID -> State UnfolderState ConfigurationID
computeEnabled es conf@Configuration{..} e = do
    s@UnfolderState{..} <- get
    let nconfevs = e:cevs                                -- the new configuration will have the events of the current configuration plus the new event e
    st <- newState cst e                                 -- compute the new state after executing the event e
    nconfID <- freshCounter $ Right ()                   -- new counter for the configuration
    aes <- mapM getEvent es                              -- get the actual enabled events from the unfolding prefix 
    ae <- getEvent e                                     -- get the added event from the unfoldng prefix
    let nconf = Configuration nconfevs st                -- the new configuration
        -- nconfigs = M.insert nconfID nconf configurations -- inserting the new configuration into the configuration map
        nconfigs = nconf -- M.insert nconfID nconf configurations -- inserting the new configuration into the configuration map
        trs = enabledTransitions system st               -- compute the enabled transitions after the new state
        ntrs = filter (\tr -> not (any (\e -> tr == (etr e)) aes && not (dependent indep tr (etr ae)))) trs  -- filter the transitions that dont have enabled events already (to avoid duplicates)
    menabledE <- trace ("(enabled, aenabled) transitions at new conf id: " ++ show nconfID ++ " " ++ show (trs,ntrs)) $ mapM (addEvents es e nconf) ntrs
    es' <- filterM (isStillEnabled e) es
    let enabledE = foldl (++) [] menabledE
        enabled' = M.insert nconfID (es' ++ enabledE) enable
    s@UnfolderState{..} <- get
    put s{configurations = nconfigs, enable = enabled'}
    return nconfID

isStillEnabled :: EventID -> EventID -> State UnfolderState Bool
isStillEnabled e e' = do
    s@UnfolderState{..} <- get
    ev <- getEvent e
    ev' <- getEvent e'
    return $ not $ dependent indep (etr ev) (etr ev')

-}
    
-- add event to the unfolding prefix
-- in general, we need to add several events 
-- es: enabled events \ [eID]
-- eID: the last event added 
-- tr: the transition that is enabled
addEvents :: EventsID -> EventID -> EventsID -> ML.TransitionID -> UnfolderOp s EventsID
addEvents es eID maxevs tr = do
  s@UnfolderState{..} <- get 
  -- @ computes the history: set of maximal events that are dependent with tr
  history <- lift $ filterM (isDependent inde evts tr) maxevs
  if null history
  then trace (show tr ++ " has empty history with max_events: " ++ show maxevs) $ return [] 
  else do
    -- eID should be a valid maximal event
    if eID `elem` history
    then if history == [eID]
         then addEvent tr history
         else do
           -- @ compute all possible histories based on the assumption that
           --   a transition cannot disable another transition. 
           --   This means that if eID is the event that enabled tr (to be checked after),
           --   then all subsequences of the history which contain eID are valid histories. 
           let history' = eID `delete` history
               histories' = map (eID:) $ subsequences history' 
               histories = history `delete` histories'
           -- @ retrieve all enabled events except eID
           evs <- lift $ mapM (\e -> getEvent "addEvents" e evts) es
           -- @ filter the events whose transition is *tr*
           let evsWtr = snd $ unzip $ filter (\(e,_) -> evtr e == tr) $ zip evs es 
           if null evsWtr
           then do 
             -- @ It is because of eID that *tr* is enabled. 
             --   Hence, all subsequences are valid histories.
             mapM (addEvent tr) histories
           else do 
             -- @ *tr* was already enabled before eID.
             --   Hence, we need to filter the histories which disable *tr* 
             -- @ compute the immediate predecessors of *evsWtr*
             -- let hges'' = map (\e -> [e' | (e',e2) <- causality, e2 == e]) ges' 
             -- hges' <- mapM (filterH tr) hges''
             -- let histories' = filter (\h -> any (\hges -> all (\e -> e `elem` h) hges) hges') histories
             -- mapM (addEvent tr) histories'
             undefined
           addEvent tr history
    else error "Unreachability to be proved"  

{-
filterH :: TransitionID -> EventsID -> State UnfolderState EventsID 
filterH tr pred = do
  s@UnfolderState{..} <- get
  let cnfpred = map (\e -> fromMaybe [] $ M.lookup e immediateConflicts) pred
  cnfpredes <- mapM (mapM getEvent) cnfpred
  return $ fst $ unzip $ filter (\(e,cfl) -> not $ any (\e' -> etr e' == tr) cfl) $ zip pred cnfpredes

-- checks if candidate is a valid history for tr considering hist = history \ {neID}
isValidHistory :: TransitionID -> EventsID -> EventsID -> State UnfolderState Bool
isValidHistory tr hist candidate = do
    s@UnfolderState{..} <- get
    let hist' = hist \\ candidate
        histcfls = map (\e -> fromMaybe [] $ M.lookup e immediateConflicts) hist'
        es = foldr intersect (head histcfls) $ tail histcfls
    ges <- mapM getEvent es
    return $ any (\e -> etr e == tr) ges || ges == [] 
-}

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
addEvent tr history =  do
  s@UnfolderState{..} <- get
  -- @ 1. Fresh event id 
  neID <- freshCounter
  -- @ 2. Compute the immediate conflicts
  -- @  a) Computes the local history of the new event 
  prede <- lift $ mapM (\e -> predecessors e evts) history        
  let localHistory = nub $ concat prede ++ history 
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
{-  
-- Compute the conflicting extensions of a configuration:
cex :: EventsID -> State UnfolderState EventsID
cex es = do 
    s@UnfolderState{..} <- get
    return $ concatMap (\e -> fromMaybe [] $ M.lookup e immediateConflicts) es

-- Compute the extensions of a configuration:
-- They are composed of the enabled events that progress the configuration
-- and the conflicting extensions which are events whose roots are in the 
-- configuration but the event is in conflict with some other event of it.
extensions :: ConfigurationID -> State UnfolderState (EventsID, EventsID)
extensions conf = do
    en <- enabled conf
    config@Configuration{..} <- getConfiguration conf
    cext <- cex cevs
    let cext' = nub cext
    trace ("conflicting extensions of conf id " ++ show conf ++ "=" ++ show cext') $ return (en,cext')

-}

-- Let e be an event of a configuration C.
--   Let cext be the conflicting extensions of C, 
--    ie. all events e' such that |-e'-| is a subset of C
--        but there is another event ê' such that e' #^ ê'
-- Let ê be the events such that e #^ ê.
-- Can ê intersect cext =!= ê? Yes. There may be immediate conflicts of 
-- e whose roots are not in C.
computePotentialAlternatives :: EventsID -> EventsID -> UnfolderOp s ()
computePotentialAlternatives maxevs evs = undefined
{- do
  s@UnfolderState{..} <- get
  foldM_ (computePotentialAlternative causality) S.empty evs where
    -- computePotentialAlternative :: Causality -> S.Set EventID -> EventID -> UnfolderOp s (S.Set EventID)
    -- @ V(e) where e is an event that has at least one imm conflict
    computePotentialAlternative causa cext e = do
      -- @ compute the events of the configuration
      let confEvs = allEventsOfConf causa maxevs
      -- @ #^(e)
      cfle <- getImmediateConflicts e
      -- @ #^(e) intersect cex(C)
      let ext = filter (isCExtension causa confEvs cext) cfle
      -- @ checks if for all e' \in ext, the alternative of e given by e' is valid
      de <- getDisabled e
      mapM_ (computeV causa maxevs de e) ext 
      return $ S.union cext $ S.fromList ext
    -- isCExtension :: Causality -> S.Set EventID -> S.Set EventID -> EventID -> Bool
    -- @ checks if a particular event is a conflicting extension: TODO: expensive
    --   verifies if there is a path from every immediate predecessor of the event
    --   to a maximal event of the configuration.
    --   Possible optimisation: compute the actual configuration! (done) 
    --    Improve the path computation based on the number of the event.
    isCExtension causa confevs cext e = 
      let prede = [ i | (i,j) <- causa, j == e] 
          check = all (\e -> S.member e confevs) prede  
      in S.member e cext || check 
    -- computeV :: Causality -> S.Set EventID -> EventsID -> EventsID -> EventID -> EventID -> UnfolderOp s ()
    computeV = undefined  
-}

{-
computeJustification :: Configuration -> EventID -> EventID -> State UnfolderState ()
computeJustification conf@Configuration{..} e e' = trace ("computingJustification of " ++ show e ++ " and "++ show e') $ do
    succe <- successors e
    prede' <- predecessors e'
    s@UnfolderState{..} <- get
    let de = fromMaybe [] $ M.lookup e disable
        v = nub $ (cevs \\ (e:succe)) ++ e':prede'
       -- v = (e':prede' ++ e:succe) \\ cevs
    check <- v `justifies` de
    isConf <- isConfiguration v
    if check && isConf -- missing a third check about the configuration and the call stack
    then trace ("adding justification " ++ show v ++ " for " ++ show e ++ " with |_e_|=" ++ show (e:succe) ++ " and [e']=" ++ show (e':prede')) $ addV v e
    else return ()

isConfiguration :: EventsID -> State UnfolderState Bool
isConfiguration es = do 
    s@UnfolderState{..} <- get
    pred <- mapM predecessors es 
    let cfns = concatMap (\e -> fromMaybe [] $ M.lookup e immediateConflicts) es
        alles = concat pred ++ es
    return $ all (\e -> elem e es) alles && not (any (\e -> elem e cfns) alles)
    
justifies :: EventsID -> EventsID -> State UnfolderState Bool
justifies v de = do
    s@UnfolderState{..} <- get
    let vcfs = map (\e -> fromMaybe [] $ M.lookup e immediateConflicts) v 
    return $ all (\e -> any (\vcf -> e `elem` vcf) vcfs) de

computeDisable :: EventID -> EventID -> State UnfolderState ()
computeDisable e ê = do
    s@UnfolderState{..} <- get
    let dê = fromMaybe [] $ M.lookup ê disable
        icê = fromMaybe [] $ M.lookup ê immediateConflicts
        dis = M.insert e (dê \\ icê) disable
    put s{ disable = dis}

computeAlternatives :: Configuration -> EventID -> State UnfolderState Alternatives
computeAlternatives conf e = trace ("computing alternatives of " ++ show e ++ " with configuration "++ show conf) $ do 
    s@UnfolderState{..} <- get
    let de = fromMaybe [] $ M.lookup e disable 
        vs = concatMap (\e' -> fromMaybe [] $ M.lookup e' alternatives) (e:de) -- As
        dec = e:de
    filterM (filterAlternatives conf dec) vs
--    cexas <- mapM cex vs 
    -- C must be subset of A
--    return $ filter (null . (dec \\)) cexas

filterAlternatives :: Configuration -> EventsID -> Alternative -> State UnfolderState Bool
filterAlternatives conf@Configuration{..} dec alts = trace ("filterAlternatives: " ++ show alts) $ do
    cexa <- cex alts
   -- ca <- mapM predecessors alts
    let decs = S.fromList dec
        c = S.fromList cevs
   --     as = concat ca ++ alts
        as = S.fromList alts 
        cexas = S.fromList cexa
    trace ("filterAlternatives: configuration " ++ show cevs ++ "; dec " ++ show dec ++ "; a " ++ show alts ++ "; cexa: " ++ show cexa ) $
     if decs `isSubsetOf` cexas
     then do 
     --  res <- secondTest cevs as 
       trace "passed first" $ if c `isSubsetOf` as -- res
                              then trace "passed second" $ return True
                              else return False
     else return False

secondTest :: EventsID -> EventsID -> State UnfolderState Bool
secondTest c alt = do 
  let calt = c `intersect` alt -- events that are in both lists 
      alt' = alt \\ calt       -- the alternatives that are not in calt
      c'   = c \\ calt         -- the events of the configuration that are not in alternative
  res <- allM (\e -> allM (isConcurrent e) alt') c'
  return $ (c' == [] && alt' == []) || res 

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f [] = return True
allM f (x:xs) = do
  y <- f x
  ys <- allM f xs
  return $ y && ys 

addV :: Alternative -> EventID -> State UnfolderState ()
addV [] e = return ()
addV alte e = do
    s@UnfolderState{..} <- get
    let aux m = case m of 
          Nothing -> Just [alte]
          Just a  -> Just $ alte:a
        alt = M.alter aux e alternatives
    put s{ alternatives = alt}
-}
