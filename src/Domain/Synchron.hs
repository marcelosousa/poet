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

run :: System -> St -> (TId, Pos) -> St
run = undefined

enabled :: System -> St -> [PEvent]
enabled = undefined

pe_name = undefined
pe_act = undefined
pe_exit_tid = undefined
pe_mut_addr = undefined
is_lock = undefined
is_unlock_of = undefined
psucc = undefined
