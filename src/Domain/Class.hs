{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Class
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- General thread analysis:
--  Naive abstract interpretation fixpoint
--  based on a worklist algorithm
-------------------------------------------------------------------------------
module Domain.Class where

import Control.Monad.State.Lazy hiding (join)
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Domain.Action
import Domain.Lattice
import Language.SimpleC.AST
import Language.SimpleC.Flow hiding (trace)
import Language.SimpleC.Util hiding (cfgs,symt)
import Model.GCS
import Util.Generic
import qualified Data.Map as M
import qualified Data.Set as S

-- | General datatypes for the collapse
type CGraph     s a = Graph  SymId () (s,a)
type CGraphs    s a = Graphs SymId () (s,a)
type ResultList s a = [(s,Pos,a)]
type NodeTable  s a = Map NodeId [(s,a)]
type WItem          = (NodeId,EdgeId,NodeId)
type Worklist       = [WItem]
         
data FixState s a =
  FixState
  {
  -- fs_mode=True returns the annotated CFG; fs_mode=False returns the final res
    fs_mode  :: Bool 
  -- thread/process/function ID 
  , fs_tid   :: TId 
  , fs_cfgs  :: CGraphs s a
  , fs_symt  :: SymbolTable
  , fs_cfg   :: CGraph s a
  -- final worklist: every edge in this worklist is a global action 
  , fs_mark  :: Set WItem
  -- map from loop heads to counter of traversals.  
  , fs_wide  :: Map NodeId Int
  -- widening level
  , fs_wid   :: Int 
  -- warnings
  , fs_warns :: Set Int
  }

type FixOp s a val = State (FixState s a) val

-- A domain is a lattice with transformers
class (Show a, Show s, Projection s, Lattice s, Lattice a, Action a) => Domain s a where
   -- State Getter
   -- state            :: FixOp s a s
   -- Enabledness Transformers
   is_enabled       :: System s a -> s -> TId -> Bool
   is_live          :: TId -> System s a -> EdgeId -> NodeId -> CGraph s a -> s -> Bool
   enabled          :: System s a -> s -> [TId]
   enabled syst st =
     let control = controlPart st
         en = M.filterWithKey (\tid pos -> is_enabled syst st tid) control
     in M.keys en
   -- Transformers for code
   code_transformer :: NodeId -> NodeId -> EdgeInfo SymId () -> s -> FixOp s a (a, s, Set Int)
   weak_update      :: NodeTable s a -> NodeId -> (s,a) -> FixOp s a (Bool, NodeTable s a)
   weak_update node_table node (st,act) = 
     case M.lookup node node_table of
       Nothing  -> return (False, M.insert node [(st,act)] node_table)
       Just lst -> case lst of
         []           -> return (False, M.insert node [(st,act)] node_table)
         [(st',act')] -> do
           let nst  = st  `join` st'
               nact = act `join` act'
           if nst == st'
           then return (True, node_table)
           else return (False, M.insert node [(nst,nact)] node_table)
         _ -> error "join_update: more than one state in the list"
   strong_update    :: NodeTable s a -> NodeId -> (s,a) -> FixOp s a (Bool, NodeTable s a)
   strong_update node_table node (st,act) =
     case M.lookup node node_table of
       Nothing  -> return (False, M.insert node [(st,act)] node_table) 
       Just lst -> case lst of
         []           -> return (False, M.insert node [(st,act)] node_table)
         [(st',act')] ->
           if st == st'
           then return (True, node_table)
           else return (False, M.insert node [(st,act `join` act')] node_table)
         _ -> error "strong_update: more than one state in the list"    
   loop_head_update :: NodeTable s a -> NodeId -> (s,a) -> FixOp s a (Bool, NodeTable s a)  
   loop_head_update node_table node (st,act) =  do
     c <- get_wide_node node
     inc_wide_node node
     w <- get_widening_level
     if c >= w 
     then mytrace False ("loop_head_update: going to apply widening") $ do 
       case M.lookup node node_table of
         -- error "loop_head_update: widening between a state and empty?" 
         Nothing ->  return $ (False, M.insert node [(st,act)] node_table)
         Just lst -> case lst of
           [] -> error "loop_head_update: widening between a state and empty?" 
           [(st',act')] ->
             let nst  = st' `widen` st
                 nact = act `join` act'
             in if nst == st'
                then return $ (True, node_table)
                else return $ (False, M.insert node [(nst,nact)] node_table)
           _ -> error "loop_head_update: widening between a state and several states?" 
     else weak_update node_table node (st,act) 
   -- Fixpoint transformers: Chaotic worklist algorithm, Determistic Run
   -- Fixpoint 
   run  :: Bool -> Int -> System s a -> s -> TId -> (Set Int,ResultList s a)
   -- Deterministic run
   drun :: Int -> System s a -> s -> (TId,Pos,SymId) -> (s,a)
   drun wid syst st (tid,pos,_) =
     let results = snd $ run True wid syst st tid
         result  = nub $ filter (\(s,p,a) -> p == pos) results
     in case result of
       []           -> error "drun: collapse does not produce dataflow fact at desired location"
       [(st,_,act)] -> (st,act)
       _ -> error $ "drun: multiple dataflow facts! pos = " ++ show pos ++ "\n" ++ show result
       
-- | API FOR STATE
get_widening_level :: FixOp s a Int
get_widening_level = do
  fs@FixState{..} <- get
  return fs_wid

add_warns :: Set Int -> FixOp s a ()
add_warns warns = do
  fs@FixState{..} <- get
  let fs_warns' = S.union fs_warns warns
  put fs { fs_warns = fs_warns' }
   
inc_wide_node :: NodeId -> FixOp s a ()
inc_wide_node node_id = do
  fs@FixState{..} <- get
  let fs_wide' = case M.lookup node_id fs_wide of
        Nothing -> M.insert node_id 1 fs_wide
        Just n  -> M.insert node_id (n+1) fs_wide
  put fs { fs_wide = fs_wide' } 

get_wide_node :: NodeId -> FixOp s a Int
get_wide_node node_id = do
  fs@FixState{..} <- get
  case M.lookup node_id fs_wide of
    Nothing -> return 0
    Just n  -> return n 

add_mark :: WItem -> FixOp s a ()
add_mark i = do 
  fs@FixState{..} <- get
  let fs_mark' = S.insert i fs_mark
  put fs { fs_mark = fs_mark' }
 
update_node_table :: NodeTable s a -> FixOp s a (CGraph s a)
update_node_table node_table' = do 
  fs@FixState{..} <- get
  let cfg = fs_cfg { node_table = node_table' }
  put fs { fs_cfg = cfg }
  return cfg

-- | main fixpoint function
fixpt :: (Show s, Show a, Domain s a) => Int -> System s a -> Bool -> TId -> CGraphs s a -> SymbolTable -> CGraph s a -> Pos -> s -> (Set Int, ResultList s a)
fixpt wid syst b tid cfgs symt cfg@Graph{..} pos st =
  mytrace False ("fixpt: tid = " ++ show tid ++ " \n" ++ show st) $ 
      -- reset the node table information with the information passed
  let node_table' = M.insert pos [(st,bot)] $ M.map (const []) node_table
      cfg'       = cfg { node_table = node_table' }
      -- initial worklist are the sucessors of the input position
      wlist      = map (\(a,b) -> (pos,a,b)) $ succs cfg' pos
      -- initial fixpoint state
      i_fix_st   = FixState b tid cfgs symt cfg' S.empty M.empty wid S.empty
  in  evalState (worklist syst wlist) i_fix_st

-- standard worklist algorithm
--  we have reached a fixpoint when the worklist is empty
worklist :: (Show s, Show a, Domain s a) => System s a -> Worklist -> FixOp s a (Set Int, ResultList s a) 
worklist syst []                        = fixpt_result
worklist syst (it@(pre,eId,post):wlist) = mytrace False ("worklist: " ++ show it) $ do
  fs@FixState{..} <- get
      -- get the current state in the pre
  let (node_st, pre_acts) = 
         case get_node_info fs_cfg pre of
           [s] -> s
           l   -> error $ "worklist invalid pre states: " ++ show l
      -- get the edge info
      e@EdgeInfo{..} = get_edge_info fs_cfg eId
  -- call the transformer on the edge
  (post_acts,ns,warns) <- code_transformer pre post e node_st
  -- add the warnings to the state
  add_warns warns
  let succs' = filter (\(a,b) -> is_live fs_tid syst a b fs_cfg ns) $ succs fs_cfg post
      rwlst = map (\(a,b) -> (post,a,b)) succs'
      -- join the previous actions with the actions of the current edge
      acts  = pre_acts `join` post_acts
  -- depending on whether the action is global or not;
  -- either add the results to the result list or update
  -- the cfg with them 
  if (?.) ns
  then worklist syst wlist
  else if isGlobal acts || (Exit `elem` edge_tags) || null rwlst 
       then do
         add_mark it
         worklist syst wlist
       else do
         -- get the node table
         let ntable = node_table fs_cfg
         -- update the node table according to the edge tags
         (is_fix,node_table') <-
           case edge_tags of
             -- loop head point 
             [LoopHead] -> loop_head_update ntable post (ns,acts)
             -- join point: join the info in the cfg 
             [IfJoin]   -> weak_update      ntable post (ns,acts) 
             -- considering everything else the standard one: just replace 
             -- the information in the cfg and add the succs of post to the worklist
             _          -> strong_update    ntable post (ns,acts) 
         cfg' <- update_node_table node_table'
         let nwlist = if is_fix then wlist else (wlist ++ rwlst)
         worklist syst nwlist
  
fixpt_result :: (Show a, Show s, Domain s a) => FixOp s a (Set Int, ResultList s a)
fixpt_result = do
  fs@FixState{..} <- get
  let marks = S.toList fs_mark
  _res <- mapM handle_mark marks
  let (ws,res) = unzip _res 
      warns = S.unions (fs_warns:ws)
  if fs_mode
  then do 
    fs@FixState{..} <- get
    let table = node_table fs_cfg
        nodes = 
           M.filterWithKey (\k _ -> not $ any (\(_,p,_) -> p == k) res) table
        nodes_res = 
           M.foldWithKey (\p l r -> map (\(a,b) -> (a,p,b)) l ++ r) [] nodes 
    return (warns, res ++ nodes_res) 
  else return (warns, res)

handle_mark :: (Show a, Show s, Domain s a) => WItem -> FixOp s a (Set Int, (s,Pos,a))
handle_mark (pre,eId,post) = do
  fs@FixState{..} <- get
  let (node_st, pre_acts) = case get_node_info fs_cfg pre of
        [s] -> s
        l   -> error $ "handle_mark invalid pre states: " ++ show l
      -- get the edge info
      e@EdgeInfo{..} = get_edge_info fs_cfg eId
  -- call the transformer on the edge
  (post_acts,st,warns) <- code_transformer pre post e node_st
  let acts = pre_acts `join` post_acts
      -- get the node table
      ntable = node_table fs_cfg
  -- update the node table according to the edge tags    
  (is_fix,node_table') <- case edge_tags of
    -- loop head point 
    [LoopHead] -> loop_head_update ntable post (st,acts)
    -- join point: join the info in the cfg 
    [IfJoin]   -> weak_update      ntable post (st,acts) 
    -- considering everything else the standard one: just replace 
    -- the information in the cfg and add the succs of post to the worklist
    _          -> strong_update    ntable post (st,acts) 
  cfg' <- update_node_table node_table'
  -- find the final result of the post
  case M.lookup post $ node_table cfg' of
    Just [(res_st,res_act)] -> do
      let rwlst = map (\(a,b) -> (post,a,b)) $ succs fs_cfg post
      if (Exit `elem` edge_tags) || null rwlst
      then return (warns, (res_st,post,res_act `join` (exit_act $ SymId fs_tid)))
      else return (warns, (res_st,post,res_act))
    _ -> error "handle_mark: unexpected value in the final node_table"
      
-- | Pretty Printing
showResultList :: (Show a, Show s) => ResultList s a -> String
showResultList l = 
 "Data Flow Information:\n" ++ (snd $ foldr (\(s,p,a) (n,r) -> 
   let s_a = "CFG Node: " ++ show p ++ "\n"
       s_r = s_a ++ show s
   in (n+1, s_r ++ r))  (1,"") l)
