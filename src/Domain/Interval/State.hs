{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.State
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- The domain for the interval semantics.
-------------------------------------------------------------------------------
module Domain.Interval.State where

import Data.Hashable
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Domain.Interval.Value
import Domain.Util
import Language.C.Syntax.Ops 
import Language.SimpleC.AST
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Debug.Trace as T

mtrace a b = b
-- | Interval Memory Cell
type IntMCell = MemCell SymId () IntValue

instance Show IntMCell where
  show (MCell _ val) = show val

-- | Interval Heap
type IntHeap = Map SymId IntMCell

-- | The interval domain 
--   The interval domain is a 
--   pair (heap, threadstate).
data IntState = 
  IntState 
  { 
    heap :: IntHeap 
  , th_states :: ThStates
  , num_th  :: Int
  , is_bot  :: Bool 
  }
  deriving Eq

h_u = "##############################################\n"
h_d = "----------------------------------------------\n"
instance Show IntState where
  show (IntState h s nt b) =
    let h_s = if M.null h 
              then "\t\tEMPTY HEAP\n" ++ h_d
              else "\t\tHEAP\n" ++ h_d ++ showIntHeap h ++ h_d
        s_s = "\t\tTHREADS(" ++ show nt ++ ")\n" ++ h_d ++ showThStates s
        i_s = h_u ++ "\t\tSTATE\n"++ h_d 
    in i_s ++ h_s ++ "\n" ++ s_s ++ h_u 

showIntHeap s = M.foldWithKey (\k m r -> "    " ++ show k ++ " := " ++ show m ++ "\n" ++ r) "" s

-- | A thread state is a control and local data
type ThStates = Map TId ThState
type Locals = Map SymId IntValue 
data ThState =
  ThState
  { 
    th_pos    :: Pos
  , th_cfg_id :: SymId 
  , th_locals :: Locals 
  } 
  deriving (Show,Eq,Ord)

-- | get the addresses of an identifier
--   super simple now by assuming not having pointers
get_addrs_id :: IntState -> Scope -> SymId -> MemAddrs
get_addrs_id st scope id = 
  case M.lookup id (heap st) of
    Nothing -> MemAddrs [MemAddr id scope] 
    Just i  -> MemAddrs [MemAddr id Global] 

-- | get_addrs retrieves the information from the 
--   points to analysis.
--   Simplify to only consider the case where the 
--   the expression is a LHS (var or array index).
get_addrs :: IntState -> Scope -> SExpression -> MemAddrs
get_addrs st scope expr = mtrace ("get_addrs: " ++ show expr) $
  case expr of
    Var id -> get_addrs_id st scope id 
    Unary CAdrOp e -> get_addrs st scope e 
    _ -> error $ "get_addrs: not supported expr " ++ show expr

get_tid_expr :: Scope -> IntState -> SExpression -> TId
get_tid_expr scope st expr = mtrace ("get_tid_expr: " ++ show expr) $
  -- get the address(es) referenced by expr
  let th_addrs = get_addrs st scope expr 
  in case read_memaddrs st th_addrs of
    [] -> error $ "get_tid: couldnt find thread for expr " ++ show expr 
    [v] -> case v of
      IntVal [VInt tid] -> tid
      _ -> error $ "get_tid: unexpected intvalue " ++ show v 
    r -> error $ "get_tid: found too many threads for expr " ++ show expr ++ " " ++ show r 

showThStates s = 
  M.foldWithKey (\k t r -> "   Thread: PID = " ++ show k ++ showThState t ++ r) "" s

showThState (ThState p c s) = 
  let p_s = ", LOC = " ++ show p
      c_s = ", CFG ID = " ++ show c
      l_s = "\n" ++ showIntHeap s
  in p_s ++ c_s ++ l_s ++ "\n" ++ h_d 

-- | Initial state which is not bottom
empty_state :: IntState 
empty_state = IntState M.empty M.empty 1 False 

-- | Set the position in the cfg of a thread
update_pc :: IntState -> TId -> Pos -> IntState
update_pc i@IntState{..} tid pos = 
  let th_st = M.update (\(ThState _ c l) -> Just $ ThState pos c l) tid th_states
  in i { th_states = th_st }

-- | Set the position in the cfg of a thread
--   This can also initialize the state of a thread
set_pos :: IntState -> TId -> SymId -> Pos -> IntState 
set_pos st@IntState{..} tid cfg_sym npos = 
  let th_st' =
        case M.lookup tid th_states of
          Nothing -> ThState npos cfg_sym M.empty 
          Just t@ThState{..} -> t { th_pos = npos }
      th_states' = M.insert tid th_st' th_states 
  in st { th_states = th_states' }

-- | Checks for state subsumption
-- 1. Check bottoms 
-- 2. Check if the number of threads
--    is greater or equal
-- 3. Check the heap
-- 4. Check the thread states
subsumes_interval :: IntState -> IntState -> Bool
subsumes_interval st1 st2 =
  case check_bottoms (is_bot st1) (is_bot st2) of
    Just r -> r
    Nothing ->
      if (num_th st1) < (num_th st2)
      then False
      else
        let sts1 = th_states st1
            hp1 = heap st1
        in if M.foldrWithKey' (\tid th b -> check_threads tid th sts1 && b) True (th_states st2)
           then M.foldrWithKey' (\mid mcell b -> check_heap mid mcell hp1 && b) True (heap st2)
           else False 
 where
   check_bottoms b1 b2 =
     if b1 
     then Just b2
     else if b2
          then Just True
          else Nothing
   check_threads tid th2 sts1 =
    case M.lookup tid sts1 of
      Nothing -> False
      Just th1 ->
       let lcs1 = th_locals th1
       in if th_pos th1 == th_pos th2
          then M.foldrWithKey' (\sym v b -> check_locals sym v lcs1 && b) True (th_locals th2) 
          else False
   check_locals :: SymId -> IntValue -> Map SymId IntValue -> Bool 
   check_locals sym val2 lcs1 =
     case M.lookup sym lcs1 of
       Nothing -> False
       Just val1 -> val2 <= val1 
   check_heap mid cell2 hp1 =
     case M.lookup mid hp1 of
       Nothing -> False
       Just cell1 ->
         let r = ty cell1 == ty cell2
             vcell al1 = val cell1
             val1 = val cell1
             val2 = val cell2
         in r && val2 <= val1
 
instance Projection IntState where
  controlPart st@IntState{..} = M.map th_pos th_states
  subsumes a b = subsumes_interval a b
  isBottom = is_bot 
  toThCFGSym st@IntState{..} tid = case M.lookup tid th_states of
    Nothing -> error $ "toThCFGSym: invalid tid " ++ show tid
    Just t@ThState{..} -> th_cfg_id 
  
join_intstate :: IntState -> IntState -> IntState
join_intstate s1 s2 = case (is_bot s1, is_bot s2) of
  (True,_) -> s2
  (_,True) -> s1
  -- They are not bot
  _ -> let _heap = (heap s1) `join_intheap` (heap s2)
           _th_states = th_states s1 `join_intthsts` th_states s2
           -- This is not correct in general
           _num_th = max (num_th s1) (num_th s2) 
           _is_bot = False
       in IntState _heap _th_states _num_th _is_bot

join_intheap :: IntHeap -> IntHeap -> IntHeap
join_intheap m1 m2 = M.unionWith join_intmcell m1 m2 

join_intthsts :: ThStates -> ThStates -> ThStates
join_intthsts = M.unionWith join_intthst 

ite :: Eq a => String -> a -> a -> a
ite e_str a b
  | a == b = a
  | otherwise = error e_str

join_intthst :: ThState -> ThState -> ThState
join_intthst t1 t2 =
  let _pos    = ite "join_intthst: diff th_pos"    (th_pos    t1) (th_pos    t2) 
  --    _id     = ite "join_intthst: diff th_id"     (th_id     t1) (th_id     t2) 
      _cfg_id = ite "join_intthst: diff th_cfg_id" (th_cfg_id t1) (th_cfg_id t2) 
      _locals = M.unionWith iJoin (th_locals t1) (th_locals t2) 
  in ThState _pos _cfg_id _locals

join_intmcell :: IntMCell -> IntMCell -> IntMCell
join_intmcell m1 m2 =
  let _ty = ty m1
      _val = (val m1) `iJoin` (val m2)
  in MCell _ty _val

-- | API for modifying the state
-- | insert_heap: inserts an element to the heap
insert_heap :: IntState -> SymId -> STy -> IntValue -> IntState
insert_heap st@IntState{..} id ty val =
  let cell = MCell ty val 
      heap' = M.insert id cell heap
  in st {heap = heap'}  

modify_heap :: IntState -> SymId -> IntValue -> IntState
modify_heap st@IntState{..} id val = 
  let heap' = M.update (update_conmcell val) id heap
  in st {heap = heap'}

update_conmcell :: IntValue -> IntMCell -> Maybe IntMCell
update_conmcell nval c@MCell{..} = Just $ c { val = nval } 

-- | insert_local: inserts an element to local state 
insert_local :: IntState -> TId -> SymId -> IntValue -> IntState
insert_local st@IntState{..} tid sym val = 
 case M.lookup tid th_states of
    Nothing -> error "insert_local: tid not found in th_states"
    Just s@ThState{..} ->
      let locals' = M.insert sym val th_locals
          s' = s { th_locals = locals' }
          th_states' = M.insert tid s' th_states
      in st { th_states = th_states' }

-- | modify the state: receives a MemAddrs and a
--   IntValue and assigns the IntValue to the MemAddrs
modify_state :: Scope -> IntState -> MemAddrs -> IntValue -> IntState
modify_state scope st addrs vals =
  case addrs of
    MemAddrTop -> error "modify_state: top addrs, need to traverse everything"
    MemAddrs l -> foldr (\a s -> modify_state' scope s a vals) st l
 where
   modify_state' :: Scope -> IntState -> MemAddr -> IntValue -> IntState
   modify_state' scope st@IntState{..} add@MemAddr{..} conval =
     -- First search in the heap 
     case M.lookup base heap of
       Nothing ->
         -- If not in the heap, search in the thread
         case scope of
           Global -> error "modify_state: id is not the heap and scope is global"
           Local i -> insert_local st i base conval 
       Just _ -> modify_heap st base conval  

-- @TODO: Return a list of a just a intvalue by doing a join over all results?
read_memaddrs :: IntState -> MemAddrs -> [IntValue]
read_memaddrs st addrs = mtrace ("read_memaddrs: " ++ show addrs) $  
  case addrs of 
    MemAddrTop -> error "read_memaddrs: have not implemented MemAddrTop"
    MemAddrs l -> map (read_memaddr st) l

read_memaddr :: IntState -> MemAddr -> IntValue
read_memaddr st addr = mtrace ("read_memaddr " ++ show addr) $
  case level addr of
    Global -> case M.lookup (base addr) (heap st) of 
      Nothing   -> error $ "read_memaddr: " ++ show addr 
      Just cell -> val cell
    Local tid -> case M.lookup tid (th_states st) of   
      Nothing -> error $ "read_memaddr: tid " ++ show tid ++ " not found with addr " ++ show addr
      Just th -> case M.lookup (base addr) (th_locals th) of
        Nothing -> error $ "read_memaddr: " ++ show addr
        Just value -> value 
 
bot_th_state :: Pos -> SymId -> ThState
bot_th_state pos cfg_id = ThState pos cfg_id M.empty

inc_num_th :: IntState -> (Int, IntState)
inc_num_th s@IntState{..} =
  let n = num_th + 1
  in (n,s { num_th = n })

insert_thread :: IntState -> SymId -> SymId -> Pos -> IntState 
insert_thread s sym cfg_sym pos =
  let (tid,s'@IntState{..}) = inc_num_th s
      th = bot_th_state pos cfg_sym
      th_states' = M.insert tid th th_states
  in s' { th_states = th_states' }

instance Hashable IntState where
  hash s@IntState{..} = hash (heap,th_states,num_th,is_bot) 
  hashWithSalt s st@IntState{..} = hashWithSalt s (heap,th_states,num_th,is_bot) 

instance Hashable IntHeap where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h
 
instance Hashable ThStates where
  hash = hash . M.toList
  hashWithSalt s th = hashWithSalt s $ M.toList th

instance Hashable ThState where
  hash th@ThState{..} = hash (th_pos,th_cfg_id,th_locals)
  hashWithSalt s th@ThState{..} = hashWithSalt s (th_pos,th_cfg_id,th_locals)

instance Hashable Locals where
  hash = hash . M.toList
  hashWithSalt s h = hashWithSalt s $ M.toList h

instance Hashable IntMCell where
  hash m@MCell{..} = hash val
  hashWithSalt s m@MCell{..} = hashWithSalt s val
 
{-
-- State equality: because I'm using a hashtable I need to stay within the ST monad
isEqual :: IntState s -> IntState s -> ST s Bool
isEqual s1 s2 = do
  l1 <- H.toList s1
  l2 <- H.toList s2
  return $ isEqual' l1 l2  
-}
