{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Synchron
-- Copyright :  (c) 2015 Marcelo Sousa
-- 
-- Modules for the synchronisation semantics
-------------------------------------------------------------------------------
module Domain.Synchron where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Haskroid.Haskroid as STID
import qualified Haskroid.Hapiroid as STID 

type TId = Integer
data Pos = 
   Active Integer
 | Lock   Integer Integer
 | Join   Integer Integer
 | Exit
 | Waiting
  deriving (Show, Eq, Ord)

data System = 
  Sys
  { stid_ptr :: STID.SteroidRef
  , poset    :: Poset
  , state    :: St 
  }

data Poset = Poset
 {
   evs_procs    :: [[PEvent]] 
 , evs_max_lock :: [STID.Event]
 }
  deriving Show

toPoset :: STID.Poset -> Poset
toPoset po =
  let o_ev_pr = STID.evs_procs po
      toPTh th tid = foldr (\e (es, enr) ->
                      let e' = toPEvent e tid enr
                      in (e':es, enr+1)) ([],0) th 
      (ev_pr,_) = foldr (\th (r, tid) ->
                      let (th',_) = toPTh th tid
                      in (th':r, tid+1)) ([],0) o_ev_pr 
  in Poset ev_pr $ STID.evs_max_lock po

toPEvent :: STID.Event -> Integer -> Integer -> PEvent
toPEvent e tid enr =
  let act = STID.ev_act e
      pre_mem_tid = STID.ev_pre_mem_tid e
      pre_mem_idx = STID.ev_pre_mem_idx e
      sidx = STID.ev_sidx e
  in PEv act tid enr pre_mem_tid pre_mem_idx sidx 

-- | A state is the sequence of positions in the partial order
type St = Map Int Pos

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
pe_name pe@PEv{..} = (tid, Active idx)

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
  M.fromList $ zip [0..] $ map (is_ready . head) evs_procs 

is_ready :: PEvent -> Pos 
is_ready p@PEv{..} 
  | pre_mem_tid == 0 && pre_mem_idx == 0 = Waiting
  | otherwise = Active 0
 
-- | Need forward pointers; otherwise it gets harder
-- assume that the transition is valid and is enabled
run :: System -> St -> (TId, Pos) -> St
run sys@Sys{..} st (_tid, Active _idx) =
  let nidx = fromInteger _idx
      ntid = fromInteger _tid
      th = (evs_procs poset) !! ntid 
      pe = th !! nidx
  in case STID.act_ty (act pe) of
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
             st'' = next_lock sys _tid _idx st addr
         in combine st' st''  
       _ -> next_proc sys st _tid (nidx + 1) 

combine :: St -> St -> St
combine = M.unionWith (max)
 
-- sets the *only* waiting lock into active
next_lock :: System -> Integer -> Integer -> St -> Integer -> St
next_lock sys p_tid p_idx st addr = 
  M.foldWithKey (\tid pos st' ->
     case pos of
       Lock a idx -> if a == addr 
                     then if check_mem_pred sys p_tid p_idx tid idx
                          then M.insert tid (Active idx) st'
                          else st'
                     else st'
       _ -> st') st st 

-- | check if p_tid, p_idx is the mem_predecessor of tid, idx 
check_mem_pred :: System -> Integer -> Integer -> Int -> Integer -> Bool
check_mem_pred sys@Sys{..} p_tid p_idx m_tid m_idx =
  let th = (evs_procs poset) !! m_tid
      ev@PEv{..} = th !! (fromInteger m_idx)
  in pre_mem_tid == p_tid && pre_mem_idx == p_idx  
   

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
                     if pre_mem_idx == prev_idx 
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
  M.foldWithKey (\tid pos pes ->
       case pos of 
         Active idx -> let e = (evs_procs poset !! tid) !! (fromInteger idx) 
                       in e:pes
         _ -> pes) [] st 
    
