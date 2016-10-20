{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Synchron
-- Copyright :  (c) 2015 Marcelo Sousa
-- 
-- Modules for the synchronisation semantics
-------------------------------------------------------------------------------
module Domain.Synchron where

import qualified Haskroid.Haskroid as STID
import qualified Haskroid.Hapiroid as STID 

type TId = Integer
type Pos = Integer

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
type St = [Pos]

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

-- | Partial order model
initial_state :: Poset -> St
initial_state p@Poset{..} = 
  map (is_ready . head) evs_procs 

is_ready :: PEvent -> Pos 
is_ready p@PEv{..} 
  | pre_mem_tid == 0 && pre_mem_idx == 0 = -1
  | otherwise = 0
 
-- | Need forward pointers; otherwise it gets harder
run :: System -> St -> (TId, Pos) -> St
run = undefined

-- | Naive procedure
enabled :: System -> St -> [PEvent]
enabled sys@Sys{..} st =
  let th_pos = zip [0..] $ map fromInteger st
  in foldr (\(tid,pos) pes ->
       if pos >= 0 && pos < length (evs_procs poset !! tid)
       then let e = (evs_procs poset !! tid) !! pos 
            in e:pes
       else pes) [] th_pos
    
