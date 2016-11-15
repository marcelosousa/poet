{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Collapse
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Collapse for interval semantics 
--  For simplicity, a thread is enabled if the control
--  state is non-negative. 
-------------------------------------------------------------------------------
module Domain.Interval.Collapse where

import Control.Monad.State.Lazy

import qualified Data.Map as M
import Data.Map (Map)

import Domain.Interval.Converter
import Domain.Interval.State
import Domain.Action
import Domain.Util
import Language.SimpleC.AST
import Language.SimpleC.Flow
import Language.SimpleC.Util
import Model.GCS
import Data.List
import qualified Debug.Trace as T

type IntGraph = Graph SymId () (IntState,Act) 
type IntGraphs = Graphs SymId () (IntState,Act) 
type Worklist = [(NodeId,EdgeId,NodeId)]
type ResultList = [(IntState,Pos,Act)]

showResultList :: ResultList -> String
showResultList l = "Data Flow Information:\n" ++ (snd $ foldr (\(s,p,a) (n,r) -> 
   let s_a = "CFG Node: " ++ show p ++ "\n"
       s_r = s_a ++ show s
   in (n+1, s_r ++ r))  (1,"") l)

instance Collapsible IntState Act where
  -- collapse :: System IntState Act -> IntState -> TId -> [(IntState,Pos,Act)]
  collapse b syst@System{..} st tid =
    let control = controlPart st
        pos = case M.lookup tid control of
          Nothing -> error $ "collapse: tid " ++ show tid ++ " is not control"
          Just p  -> p
        th_sym = SymId tid
        th_cfg = case M.lookup th_sym cfgs of
          Nothing -> error $ "collapse: cant find thread " ++ show th_sym
          Just cfg -> cfg 
    in gen_collapse b tid cfgs symt th_cfg pos st

-- @TODO: Receive other options for widening and state splitting
gen_collapse :: Bool -> TId -> IntGraphs -> SymbolTable -> IntGraph -> Pos -> IntState -> ResultList 
gen_collapse b tid cfgs symt cfg@Graph{..} pos st =
  -- reset the node table information with the information passed
  let node_table' = M.insert pos [(st,bot_act)] $ M.map (const []) node_table
      cfg' = cfg { node_table = node_table' }
      wlist = map (\(a,b) -> (pos,a,b)) $ succs cfg' pos
  in  worklist b tid cfgs symt cfg' wlist [] 

worklist :: Bool -> TId -> IntGraphs -> SymbolTable -> IntGraph -> Worklist -> ResultList -> ResultList 
worklist b tid cfgs symt cfg@Graph{..} wlist res =
  case wlist of
    [] -> if b 
          then M.foldWithKey (\p l r -> map (\(a,b) -> (a,p,b)) l ++ r) [] node_table 
          else res
    ((pre,eId,post):wlst) ->
          -- get the current state in the pre
      let l_st = get_node_info cfg pre
          node_st = case l_st of
                 [] -> error "worklist_fix: bottom state not supposed to happen"
                 [s] -> s
                 _ -> error "worklist_fix: more than one abstract state in the node" 
          -- get the edge info
          e@EdgeInfo{..} = get_edge_info cfg eId
          -- construct the transformer state
          tr_st = IntTState (Local tid) (fst node_st) symt cfgs (is_cond edge_tags)
          -- decide based on the type of edge which transformer to call
          (acts,ns@IntTState{..}) = case edge_code of
              D decl -> runState (transformer_decl decl) tr_st 
                -- execute the transformer
              E expr -> runState (transformer_expr expr) tr_st
      -- depending on whether the action is global or not;
      -- either add the results to the result list or update
      -- the cfg with them 
      in if isGlobal acts -- || (Exit `elem` edge_tags)
         then worklist b tid cfgs symt cfg wlst $ nub ((st,post,acts):res)
         else 
           -- depending on the tags of the edge; the behaviour is different
           let (is_fix,node_table') =
                if is_join edge_tags
                -- join point: join the info in the cfg 
                then join_update node_table post (st,acts) 
                -- considering everything else the standard one: just replace 
                -- the information in the cfg and add the succs of post to the worklist
                else strong_update node_table post (st,acts) 
               cfg' = cfg { node_table = node_table' }
               rwlst = map (\(a,b) -> (post,a,b)) $ succs cfg' post
               nwlist = if is_fix then wlst else (wlst ++ rwlst)
           in worklist b tid cfgs symt cfg' nwlist res

join_update :: Map NodeId [(IntState,Act)] -> NodeId -> (IntState,Act) -> (Bool, Map NodeId [(IntState,Act)])
join_update node_table node (st,act) =
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [(st,act)] node_table)
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        let nst = st `join_intstate` st'
            nact = act `join_act` act'
        in if nst == st'
           then (True, node_table)
           else (False, M.insert node [(nst,nact)] node_table)
      _ -> error "join_update: more than one state in the list"
 
strong_update :: Map NodeId [(IntState,Act)] -> NodeId -> (IntState,Act) -> (Bool, Map NodeId [(IntState,Act)])
strong_update node_table node (st,act) =
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [(st,act)] node_table) 
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        if st == st'
        then (True, node_table)
        else (False, M.insert node [(st,act)] node_table)
      _ -> error "strong_update: more than one state in the list" 
