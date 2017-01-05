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
import Domain.Util
import Model.GCS

-- Default implementation of an
-- Action using read write sets
-- over the memory addresses 
-- accessed.
data Act
  = Act
  { rds     :: MemAddrs -- read
  , wrs     :: MemAddrs -- write
  , locks   :: MemAddrs -- locks
  , unlocks :: MemAddrs -- unlocks
  , tcreate :: MemAddrs -- phtread_create 
  , tjoin   :: MemAddrs -- phtread_join
  , texit   :: MemAddrs -- phtread_exit
  }
  deriving (Eq,Ord)

instance Show Act where
  show (Act r w l u c j e) = 
    "Act { r = " ++ show r ++ ", w = " ++ show w ++ ", lk = " ++ show l ++ ", ulk = " ++ show u ++ ", c = " ++ show c ++ ", j = " ++ show j ++ ", e = " ++ show e
 
bot_act :: Act
bot_act =
  Act bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs

read_act :: SymId -> Scope -> Act
read_act sym scope = Act (MemAddrs [MemAddr sym scope]) bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs

write_act :: SymId -> Scope -> Act
write_act sym scope = Act bot_maddrs (MemAddrs [MemAddr sym scope]) bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs

create_thread_act :: SymId -> Act
create_thread_act tid = 
  Act bot_maddrs bot_maddrs bot_maddrs bot_maddrs (MemAddrs [MemAddr tid Global]) bot_maddrs bot_maddrs

join_thread_act :: SymId -> Act
join_thread_act tid = 
  Act bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs (MemAddrs [MemAddr tid Global]) bot_maddrs 

exit_thread_act :: SymId -> Act
exit_thread_act tid = 
  Act bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs bot_maddrs (MemAddrs [MemAddr tid Global]) 
 
join_act :: Act -> Act -> Act
join_act a1 a2 =
  let r = rds a1 `join_maddrs` rds a2 
      w = wrs a1 `join_maddrs` wrs a2 
      l = locks a1 `join_maddrs` locks a2 
      u = unlocks a1 `join_maddrs` unlocks a2
      c = tcreate a1 `join_maddrs` tcreate a2 
      j = tjoin  a1 `join_maddrs` tjoin a2
      e = texit a1 `join_maddrs` texit a2
  in Act r w l u c j e 

add_writes :: MemAddrs -> Act -> Act
add_writes ws act@Act{..} =
  let wrs' = ws `join_maddrs` wrs
  in act { wrs = wrs' }
  
{-
instance Show Act where
  show act@Act{..} =
    let rs = "reads: " ++ show rds
        wrds = "writes: " ++ show wrs
        lks = "locks: " ++ show locks
        ulks = "unlocks: " ++ show unlocks
    in rs++"\n"++wrds++"\n"++lks++"\n"++ulks
-}

instance Action Act where
  isBlocking act@Act{..} = 
    not (is_maddrs_bot locks && is_maddrs_bot unlocks)
  isJoin act@Act{..} = 
    not (is_maddrs_bot tjoin) 
  isUnlockOf a1 a2 =
    let ulks2 = unlocks a2
        f addr = is_maddrs_bot $ meet_maddrs addr ulks2 
        r = f $ rds a1
        w = f $ wrs a1
        l = f $ locks a1 
        u = f $ unlocks a1
    in l && w && r && u 
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
    in is_global acts || not (all is_maddrs_bot [locks,unlocks,tcreate,tjoin])
  isCreateOf tid_sym a1@Act{..} =
    case tcreate of
      MemAddrTop -> True
      MemAddrs at -> any (\a -> a == MemAddr tid_sym Global) at 

is_global :: MemAddrs -> Bool
is_global maddr = case maddr of
  MemAddrTop -> True
  MemAddrs l -> any (\a@MemAddr{..} -> level == Global) l

act_addrs :: Act -> MemAddrs
act_addrs a@Act{..} =
  rds `join_maddrs` wrs `join_maddrs` locks `join_maddrs` 
  unlocks `join_maddrs` tcreate `join_maddrs` tjoin  `join_maddrs` texit
