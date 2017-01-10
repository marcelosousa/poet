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
import Domain.MemAddr
import Domain.Util
import Model.GCS
import qualified Debug.Trace as T

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
 
bot_act :: Act v
bot_act =
  Act bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs

read_act :: SymId -> v -> Scope -> Act v
read_act sym offset scope = Act (MemAddrs [MemAddr sym offset scope]) bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs

read_act_addr :: MemAddrs v -> Act v
read_act_addr addr = Act addr bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs

write_act :: SymId -> v -> Scope -> Act v
write_act sym offset scope = Act bot_maddrs (MemAddrs [MemAddr sym offset scope]) bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs

write_act_addr :: MemAddrs v -> Act v
write_act_addr addr = Act bot_maddrs addr bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs

lock_act_addr :: MemAddrs v -> Act v
lock_act_addr addr = Act bot_maddrs bot_maddrs addr bot_maddrs bot_maddrs bot_maddrs bot_maddrs

unlock_act_addr :: MemAddrs v -> Act v
unlock_act_addr addr = Act bot_maddrs bot_maddrs bot_maddrs addr bot_maddrs bot_maddrs bot_maddrs

create_thread_act :: SymId -> v -> Act v
create_thread_act tid offset = 
  Act bot_maddrs bot_maddrs bot_maddrs bot_maddrs (MemAddrs [MemAddr tid offset Global]) bot_maddrs bot_maddrs

join_thread_act :: SymId -> v -> Act v
join_thread_act tid offset = 
  Act bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs (MemAddrs [MemAddr tid offset Global]) bot_maddrs 

exit_thread_act :: SymId -> v -> Act v
exit_thread_act tid offset = 
  Act bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs (MemAddrs [MemAddr tid offset Global]) 
 
join_act :: (Eq v, Show v) => Act v -> Act v -> Act v
join_act a1 a2 =
  let r = rds a1 `join_maddrs` rds a2 
      w = wrs a1 `join_maddrs` wrs a2 
      l = locks a1 `join_maddrs` locks a2 
      u = unlocks a1 `join_maddrs` unlocks a2
      c = tcreate a1 `join_maddrs` tcreate a2 
      j = tjoin  a1 `join_maddrs` tjoin a2
      e = texit a1 `join_maddrs` texit a2
  in Act r w l u c j e 

add_writes :: Eq v => MemAddrs v -> Act v -> Act v
add_writes ws act@Act{..} =
  let wrs' = ws `join_maddrs` wrs
  in act { wrs = wrs' }
  
instance Eq v => Action (Act v) where
  isUnlock act@Act{..} = 
    not $ is_maddrs_bot unlocks
  isLock act@Act{..} = 
    not $ is_maddrs_bot locks 
  isJoin act@Act{..} = 
    not $ is_maddrs_bot tjoin
  isUnlockOf a1 a2 =
    let ulks2 = unlocks a2
        lks1 = locks a1
    in not $ is_maddrs_bot $ meet_maddrs lks1 ulks2 
  isLockOf a1 a2 = 
    let lks2 = locks a2
        f addr = is_maddrs_bot $ meet_maddrs addr lks2 
        r = f $ rds a1
        w = f $ wrs a1
        l = f $ locks a1 
        u = f $ unlocks a1
    in u && w && r && l 
  interferes a1 a2 =
    let a1_addrs = act_addrs a1
        a2_addrs = act_addrs a2
    in not $ is_maddrs_bot $ meet_maddrs a1_addrs a2_addrs
  isGlobal act@Act{..} =
    let acts = act_addrs act
    -- version where any access to the heap is considered a global action
    -- in is_global acts || not (all is_maddrs_bot [locks,unlocks,tcreate,tjoin])
    in not (all is_maddrs_bot [locks,unlocks,tcreate,tjoin])
  isCreateOf tid_sym a1@Act{..} =
    case tcreate of
      MemAddrTop -> True
      MemAddrs at -> any (\a -> case a of 
         MemAddr tid_sym' _ Global -> tid_sym == tid_sym' 
         _ -> False) at 

act_addrs :: Eq v => Act v -> MemAddrs v
act_addrs a@Act{..} =
  rds `join_maddrs` wrs `join_maddrs` locks `join_maddrs` 
  unlocks `join_maddrs` tcreate `join_maddrs` tjoin  `join_maddrs` texit
