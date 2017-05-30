{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Action 
-- Copyright :  (c) 2016 Marcelo Sousa
-- Provides the implementation of the interface
-- to compute on-the-fly independece based on
-- read-write sets.
-------------------------------------------------------------------------------
module Domain.Action where

import Language.SimpleC.AST
import Domain.Lattice
import Domain.MemAddr
import Domain.Util
import Model.GCS
import qualified Debug.Trace as T

-- Action defines the API to compute independence based on actions
class (Eq act) => Action act where
  isLock     :: act -> Bool
  isUnlock   :: act -> Bool
  isJoin     :: act -> Bool
  -- Given two sets of actions a1 and a2,
  -- check if there exists in a2 an unlock 
  -- or lock with an address that is touched
  -- by a1.
  isUnlockOf :: act -> act -> Bool 
  isLockOf   :: act -> act -> Bool
  isCreateOf :: SymId -> act -> Bool 
  -- Two sets of actions are independent
  interferes :: act -> act -> Bool
  isGlobal   :: act -> Bool
  -- Builders
  exit_act   :: SymId -> act

-- Default implementation of an
-- Action using read write sets
-- over the memory addresses 
-- accessed.
data Act v
  = Act
  { rds     :: MemAddrs v -- read
  , wrs     :: MemAddrs v -- write
  , locks   :: MemAddrs v -- locks
  , unlocks :: MemAddrs v -- unlocks
  , tcreate :: MemAddrs v -- phtread_create 
  , tjoin   :: MemAddrs v -- phtread_join
  , texit   :: MemAddrs v -- phtread_exit
  }
  deriving (Eq,Ord)
      
instance Show v => Show (Act v) where
  show (Act r w l u c j e) = 
    "Act { r = " ++ show r ++ ", w = " ++ show w ++ ", lk = " ++ show l ++ ", ulk = " ++ show u ++ ", c = " ++ show c ++ ", j = " ++ show j ++ ", e = " ++ show e

read_act :: Ord v => SymId -> v -> Scope -> Act v
read_act sym offset scope = Act (MemAddrs [MemAddr sym offset scope]) bot bot bot bot bot bot

read_act_addr :: Ord v => MemAddrs v -> Act v
read_act_addr addr = Act addr bot bot bot bot bot bot

write_act :: Ord v => SymId -> v -> Scope -> Act v
write_act sym offset scope = Act bot (MemAddrs [MemAddr sym offset scope]) bot bot bot bot bot

write_act_addr :: Ord v => MemAddrs v -> Act v
write_act_addr addr = Act bot addr bot bot bot bot bot

lock_act_addr :: Ord v => MemAddrs v -> Act v
lock_act_addr addr = Act bot bot addr bot bot bot bot

unlock_act_addr :: Ord v => MemAddrs v -> Act v
unlock_act_addr addr = Act bot bot bot addr bot bot bot

create_thread_act :: Ord v => SymId -> v -> Act v
create_thread_act tid offset = 
  Act bot bot bot bot (MemAddrs [MemAddr tid offset Global]) bot bot

join_thread_act :: Ord v => SymId -> v -> Act v
join_thread_act tid offset = 
  Act bot bot bot bot bot (MemAddrs [MemAddr tid offset Global]) bot 

exit_thread_act :: ToValue v => SymId -> Act v
exit_thread_act tid = 
  Act bot bot bot bot bot bot (MemAddrs [MemAddr tid zero Global])
 
add_writes :: ToValue v => MemAddrs v -> Act v -> Act v
add_writes ws act@Act{..} =
  let wrs' = ws `join` wrs
  in act { wrs = wrs' }
  
instance ToValue v => Action (Act v) where
  isUnlock act@Act{..} = 
    not $ (?.) unlocks
  isLock act@Act{..} = 
    not $ (?.) locks 
  isJoin act@Act{..} = 
    not $ (?.) tjoin
  isUnlockOf a1 a2 =
    let ulks2 = unlocks a2
        lks1 = locks a1
    in not $ (?.) $ lks1 `meet` ulks2 
  isLockOf a1 a2 = 
    let lks2 = locks a2
        f addr = (?.) $ addr `meet` lks2 
        r = f $ rds a1
        w = f $ wrs a1
        l = f $ locks a1 
        u = f $ unlocks a1
    in u && w && r && l 
  interferes a1 a2 =
    let a1_addrs = act_addrs a1
        a2_addrs = act_addrs a2
    in not $ (?.) $ a1_addrs `meet` a2_addrs
  isGlobal act@Act{..} =
    let acts = act_addrs act
    -- version where any access to the heap is considered a global action
    in is_global acts || not (all (?.) [locks,unlocks,tcreate,tjoin])
    -- in not (all (?.) [locks,unlocks,tcreate,tjoin])
  isCreateOf tid_sym a1@Act{..} =
    case tcreate of
      MemAddrTop -> True
      MemAddrs at -> any (\a -> case a of 
         MemAddr tid_sym' _ Global -> tid_sym == tid_sym' 
         _ -> False) at
  exit_act = exit_thread_act

instance ToValue v => Lattice (Act v) where
   bot = Act bot bot bot bot bot bot bot
   top = Act top top top top top top top
   join a b = 
     let r = rds     a `join` rds b 
         w = wrs     a `join` wrs b 
         l = locks   a `join` locks b 
         u = unlocks a `join` unlocks b
         c = tcreate a `join` tcreate b 
         j = tjoin   a `join` tjoin b
         e = texit   a `join` texit b
     in Act r w l u c j e 
   meet a b = 
     let r = rds     a `meet` rds b 
         w = wrs     a `meet` wrs b 
         l = locks   a `meet` locks b 
         u = unlocks a `meet` unlocks b
         c = tcreate a `meet` tcreate b 
         j = tjoin   a `meet` tjoin b
         e = texit   a `meet` texit b
     in Act r w l u c j e 
      
act_addrs :: ToValue v => Act v -> MemAddrs v
act_addrs a@Act{..} = foldr join bot [rds,wrs,locks,unlocks,tcreate,tjoin,texit]