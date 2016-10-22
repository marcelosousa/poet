{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Synchron
-- Copyright :  (c) 2015 Marcelo Sousa
-- 
-- Modules for the synchronisation semantics
-------------------------------------------------------------------------------
module Domain.Synchron where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Haskroid.Haskroid as STID
import qualified Haskroid.Hapiroid as STID 
import qualified Debug.Trace as T

type TId = Integer
type Pos = Integer
{- 
   Active Integer
 | Lock   Integer Integer
 | Join   Integer Integer
 | Exit
 | Waiting
  deriving (Show, Eq, Ord)
-}

data System = 
  Sys
  { stid_ptr :: STID.SteroidRef
  , poset    :: Poset
  , state    :: St 
  }
  deriving Show

data Poset = Poset
 {
   evs_procs    :: [[PEvent]] 
 , evs_max_lock :: [STID.Event]
 }

instance Show Poset where
  show p@Poset{..} =
    foldr (\(th,tid) r -> 
      "Thread " ++ show tid ++ "\n"
   ++ (foldr (\(e,idx) r' -> "eventt: " ++ show idx ++ " " ++ show e ++ "\n" ++ r') "" $ zip th [0..]) ++ r) "" $ zip evs_procs [0..]

toPoset :: STID.Poset -> Poset
toPoset po =
  let o_ev_pr = STID.evs_procs po
      toPTh th tid = foldl (\(es, enr) e ->
                      let e' = toPEvent e tid enr
                      in (M.insert enr e' es, enr+1)) (M.empty,0) th 
      (ev_pr,_) = foldl (\(r, tid) th ->
                      let (th',_) = toPTh th tid
                      in (M.insert tid th' r, tid+1)) (M.empty,0) o_ev_pr 
  in Poset (toFlat $ toFPoset ev_pr) $ STID.evs_max_lock po

-- Forward poset
toFPoset :: M.Map Integer (M.Map Integer PEvent) -> M.Map Integer (M.Map Integer PEvent) 
toFPoset m = M.foldWithKey (\tid th m' -> 
              M.foldWithKey (updatePEvent tid) m' th)  m m 

updatePEvent :: Integer -> Integer -> PEvent -> M.Map Integer (M.Map Integer PEvent) -> M.Map Integer (M.Map Integer PEvent)
updatePEvent succ_tid succ_idx e@PEv{..} m = -- T.trace ("updatePEvent " ++ show (succ_tid, succ_idx)) $
  let m_th = if succ_idx == 0
             then m
             else updatePoset (succ_tid, (succ_idx-1)) (succ_tid, succ_idx) m
  in if pre_mem_tid == 0 && pre_mem_idx == 0
     then m_th
     else updatePoset (pre_mem_tid, pre_mem_idx) (succ_tid, succ_idx) m_th 

updatePoset :: (Integer, Integer) -> (Integer, Integer) -> M.Map Integer (M.Map Integer PEvent) -> M.Map Integer (M.Map Integer PEvent)
updatePoset (pre_tid, pre_idx) (succ_tid, succ_idx) m =
  case M.lookup pre_tid m of
    Nothing -> error "updatePoset"
    Just th -> case M.lookup pre_idx th of
      Nothing -> error "updatePoset"
      Just e@PEv{..} ->
        let _imm_succ = (succ_tid,succ_idx):imm_succ
            e' = e {imm_succ = _imm_succ}
        in M.insert pre_tid (M.insert pre_idx e' th) m 

toFlat :: M.Map Integer (M.Map Integer PEvent) -> [[PEvent]]
toFlat m = map M.elems $ M.elems m
 
toPEvent :: STID.Event -> Integer -> Integer -> PEvent
toPEvent e tid enr =
  let act = STID.ev_act e
      pre_mem_tid = STID.ev_pre_mem_tid e
      pre_mem_idx = STID.ev_pre_mem_idx e
      sidx = STID.ev_sidx e
  in PEv act tid enr pre_mem_tid pre_mem_idx [] sidx 

-- | A state is the sequence of positions in the partial order
-- type St = Map Int Pos
-- A state is a map from process to (cur_pos, map from idx to dots)
type St = Map Int (Int, Map Int Int)

show_st :: St -> String
show_st st = 
  M.foldWithKey (\tid (c_idx, m) r -> 
       "thread " ++ show tid ++ ", position " ++ show c_idx
    ++ ". colors: " ++ (M.foldWithKey (\idx c r -> 
       show (idx,c) ++ ", " ++ r) "" m) ++ "\n" ++ r) "" st 

-- | An action is the same as the low-level
type Act = STID.Action
 
-- | Pre-Event is an extension of the event in the underlying 
--   partial order
data PEvent = PEv
 {
   act         :: Act     -- ^ action
 , tid         :: Integer -- ^ thread of ev
 , idx         :: Integer -- ^ index in thread 
 , pre_mem_tid :: Integer -- ^ thread of mem pre
 , pre_mem_idx :: Integer -- ^ index in thread of mem pre
 , imm_succ    :: [(Integer, Integer)] -- ^ immediate successors
 , sidx        :: Integer -- ^ index in the stream
 }
  deriving (Show, Ord, Eq)

can_have_two_pred :: Act -> Bool
can_have_two_pred a@STID.Act{..} =
  case act_ty of
    STID.JOIN -> True
    STID.LOCK -> True
    _         -> False

pe_name :: PEvent -> (TId, Pos)
pe_name pe@PEv{..} = (tid, idx)

pe_act :: PEvent -> Act 
pe_act pe@PEv{..} = act 

pe_exit_tid :: Act -> Integer
pe_exit_tid = STID.act_val 

pe_mut_addr :: Act -> Integer
pe_mut_addr = STID.act_addr 

is_lock :: PEvent -> Bool
is_lock pe@PEv{..} =
  case STID.act_ty act of
    STID.LOCK -> True
    _         -> False 

lock_addr :: PEvent -> Integer 
lock_addr pe@PEv{..} = STID.act_addr act 

is_unlock :: Act -> Bool
is_unlock a@STID.Act{..} =
  case act_ty of
    STID.UNLOCK -> True
    _ -> False
 
is_begin_act_of :: Act -> Integer -> Integer -> Bool
is_begin_act_of a@STID.Act{..} t t' =
  case act_ty of
    STID.ENTRY -> t == t' 
    _ -> False

is_unlock_act_of :: Act -> Integer -> Bool
is_unlock_act_of a@STID.Act{..} addr =
  case act_ty of
    STID.UNLOCK -> act_addr == addr 
    _ -> False

is_lock_act_of :: Act -> Integer -> Bool
is_lock_act_of a@STID.Act{..} addr =
  case act_ty of
    STID.LOCK -> act_addr == addr 
    _ -> False

is_exit :: Act -> Bool
is_exit a@STID.Act{..} =
  case act_ty of
    STID.EXIT -> True
    _ -> False

-- | Partial order model
initial_state :: Poset -> St
initial_state p@Poset{..} =
  M.fromList $ zip [0..] $ map (const (0, M.singleton 0 1)) evs_procs
  -- M.fromList $ zip [0..] $ map (is_ready . head) evs_procs 

run :: System -> St -> (TId, Pos) -> St
run sys@Sys{..} st (_tid, _idx) = 
  let nidx = fromInteger _idx
      ntid = fromInteger _tid
      th = (evs_procs poset) !! ntid 
      pe = th !! nidx
      succ = imm_succ pe
      st_th = case M.lookup ntid st of
               Nothing -> error "run: not possible"
               Just (p,m) -> M.insert ntid (p+1,m) st
  in foldr (\(_s_tid, _s_idx) m ->
       let s_idx = fromInteger _s_idx
           s_tid = fromInteger _s_tid
       in case M.lookup s_tid m of
            Nothing -> error "run: thread not in state"
            Just (p,nm) ->
              let nm' = case M.lookup s_idx nm of
                   Nothing -> M.insert s_idx 1 nm
                   Just c  -> M.insert s_idx (c+1) nm
              in M.insert s_tid (p,nm') m) st_th succ 

enabled :: System -> St -> [PEvent]
enabled sys@Sys{..} st = 
  M.foldWithKey (\tid (idx,m) pes ->
       let th = evs_procs poset !! tid
       in if length th <= idx || idx < 0
          then pes 
          else let e = th !! idx
               in if (pre_mem_tid e) == 0 && (pre_mem_idx e) == 0
                  then e:pes
                  else case M.lookup idx m of
                    Nothing -> pes -- error $ "enabled: should not happen"
                    Just x  -> if x == 2
                               then e:pes
                               else pes) [] st 
  --- in T.trace ("enabled: st " ++ show st ++ ", " ++ show res) $ res 
{-
is_ready :: PEvent -> Pos 
is_ready p@PEv{..} 
  | pre_mem_tid == 0 && pre_mem_idx == 0 = Active 0 
  | otherwise = Waiting 
 
-- | Need forward pointers; otherwise it gets harder
-- assume that the transition is valid and is enabled
run :: System -> St -> (TId, Pos) -> St
run sys@Sys{..} st (_tid, Active _idx) = T.trace ("run " ++ show st) $ 
  let nidx = fromInteger _idx
      ntid = fromInteger _tid
      th = (evs_procs poset) !! ntid 
      pe = th !! nidx
  in T.trace ("run " ++ show pe) $ case STID.act_ty (act pe) of
       -- we have reached at the end of the thread
       -- confirm that the act is of an TEXIT
       -- we have two possibilities: 
       -- 1) this event has a memory sucessor
       -- 2) this event does not have a memory sucessor
       -- we have two possibilities: 
       -- 1) the thread successor is not enabled because the memory
       -- predecessor has not been fired
       -- in that case disable the thread 
       -- 2) the thread successor is enabled 
       STID.EXIT -> 
         let st' = M.insert ntid Exit st
         -- check if there is any thread waiting to JOIN on this tid
         in case check_join st _tid of
             Nothing -> st' 
             Just (ntid, nidx) -> M.insert ntid (Active nidx) st' 
       STID.CREATE ->
         let st' = next_proc sys st _tid (nidx + 1)
             val = fromInteger $ STID.act_val (act pe)
         in M.insert val (Active 0) st'
       STID.UNLOCK ->
         let st'  = next_proc sys st _tid (nidx + 1)
             addr = STID.act_addr (act pe)
             st'' = next_lock sys _tid _idx st' addr
         in st'' -- combine st' st''
       STID.LOCK ->
         let st'  = next_proc sys st _tid (nidx + 1)
             addr = STID.act_addr (act pe)
         in disable sys st' addr
       _ -> next_proc sys st _tid (nidx + 1) 

-- | disable any lock that might be enabled
disable :: System -> St -> Integer -> St
disable sys@Sys{..} st addr =
  M.foldWithKey (\_tid pos st' ->
     case pos of
       Active idx -> 
         let nidx = fromInteger idx
             th = (evs_procs poset) !! _tid 
             pe = th !! nidx
         in case STID.act_ty (act pe) of
           STID.LOCK ->
             let a = STID.act_addr (act pe)
             in if a == addr 
                then M.insert _tid (Lock addr idx) st'
                else st'
           _ -> st'
       _ -> st') st st 

combine :: St -> St -> St
combine = M.unionWith (max)
 
-- sets the *only* waiting lock into active
next_lock :: System -> Integer -> Integer -> St -> Integer -> St
next_lock sys p_tid p_idx st addr = 
  M.foldWithKey (\tid pos st' ->
     case pos of
       Lock a idx -> if a == addr 
                     then M.insert tid (Active idx) st'
                    -- then if check_mem_pred sys p_tid p_idx tid idx
                    --      then M.insert tid (Active idx) st'
                    --      else st'
                     else st'
       _ -> st') st st 

-- | check if p_tid, p_idx is the mem_predecessor of tid, idx 
check_mem_pred :: System -> Integer -> Integer -> Int -> Integer -> Bool
check_mem_pred sys@Sys{..} p_tid p_idx m_tid m_idx =
  let th = (evs_procs poset) !! m_tid
      ev@PEv{..} = th !! (fromInteger m_idx)
  in pre_mem_tid == p_tid --  && pre_mem_idx == p_idx  
   

-- checks if the next event in the process is enabled
next_proc :: System -> St -> TId -> Int -> St
next_proc sys@Sys{..} st m_tid m_idx_i = 
  let m_tid_i = fromInteger m_tid
      m_idx = toInteger m_idx_i
      th = (evs_procs poset) !! m_tid_i 
      ev@PEv{..} = th !! m_idx_i
  in case STID.act_ty act of
        -- An event can only be disabled if it is a lock or a join
        STID.LOCK -> 
          if pre_mem_idx == 0 && pre_mem_tid == 0
          then M.insert m_tid_i (Active m_idx) st 
          else case M.lookup (fromInteger pre_mem_tid) st of
                 Nothing  -> error "next_proc: cant find state for tid" 
                 Just pos -> case pos of
                   Active prev_idx ->
                     if pre_mem_idx <= prev_idx -- CHECK THAT THIS MAKES SENSE 
                     then M.insert m_tid_i (Active m_idx) st 
                     else M.insert m_tid_i (Lock (STID.act_addr act) m_idx) st
                   Join _ prev_idx ->
                     if pre_mem_idx <= prev_idx -- CHECK THAT THIS MAKES SENSE 
                     then M.insert m_tid_i (Active m_idx) st 
                     else M.insert m_tid_i (Lock (STID.act_addr act) m_idx) st
                   _ -> M.insert m_tid_i (Lock (STID.act_addr act) m_idx) st 
        STID.JOIN -> 
          case M.lookup (fromInteger pre_mem_tid) st of
            Nothing  -> error "next_proc: cant find state for tid" 
            Just pos -> case pos of
              Exit -> M.insert m_tid_i (Active m_idx) st 
              _ -> M.insert m_tid_i (Join (STID.act_val act) m_idx) st 
        _ -> M.insert m_tid_i (Active m_idx) st
                  
check_join :: St -> Integer -> Maybe (Int, Integer)
check_join st i =
  M.foldWithKey (\tid pos r ->
    case pos of
      Join j idx -> if i == j then Just (tid, idx) else r
      _ -> r) Nothing st 

-- | Naive procedure
enabled :: System -> St -> [PEvent]
enabled sys@Sys{..} st =
  let res = M.foldWithKey (\tid pos pes ->
       case pos of 
         Active idx -> let e = (evs_procs poset !! tid) !! (fromInteger idx) 
                       in e:pes
         _ -> pes) [] st 
  in T.trace ("enabled from state " ++ show st ++ ", enabled = " ++ show res) $ res 
-}
