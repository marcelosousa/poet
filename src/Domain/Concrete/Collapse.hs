{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Collapse
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Collapse for concrete semantics 
--  For simplicity, a thread is enabled if the control
--  state is non-negative. 
-------------------------------------------------------------------------------
module Domain.Concrete.Collapse where

import Control.Monad.State.Lazy

import qualified Data.Map as M
import Data.Map (Map)

import Domain.Concrete.Action
import Domain.Concrete.Converter
import Domain.Concrete.State
import Language.SimpleC.AST
import Language.SimpleC.Flow
import Language.SimpleC.Util
import Model.GCS

type ConGraph = Graph SymId () (CState,Act) 
type ConGraphs = Graphs SymId () (CState,Act) 
type Worklist = [(NodeId,EdgeId,NodeId)]
type ResultList = [(CState,Pos,Act)]

instance Collapsible CState Act where
  -- collapse :: System CState Act -> CState -> TId -> [(CState,Pos,Act)]
  collapse syst@System{..} st tid =
    let control = controlPart st
        pos = case M.lookup tid control of
          Nothing -> error "collapse: tid is not represented in the control"
          Just p  -> p
        th_sym = SymId tid
        th_cfg = case M.lookup th_sym cfgs of
          Nothing -> error "collapse: cant find thread"
          Just cfg -> cfg 
    in gen_collapse tid cfgs symt th_cfg pos st

-- @TODO: Receive other options for widening and state splitting
gen_collapse :: TId -> ConGraphs -> SymbolTable -> ConGraph -> Pos -> CState -> ResultList 
gen_collapse tid cfgs symt cfg@Graph{..} pos st =
  -- reset the node table information with the information passed
  let node_table' = M.insert pos [(st,bot_act)] $ M.map (const []) node_table
      wlist = map (\(a,b) -> (pos,a,b)) $ succs cfg pos
  in worklist_fix tid cfgs symt cfg wlist [] 

worklist_fix :: TId -> ConGraphs -> SymbolTable -> ConGraph -> Worklist -> ResultList -> ResultList 
worklist_fix tid cfgs symt cfg@Graph{..} wlist res =
  case wlist of
    [] -> res 
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
          tr_st = ConTState (Local tid) (fst node_st) bot_sigma symt cfgs (is_cond edge_tags)
          -- decide based on the type of edge which transformer to call
          (acts,ns@ConTState{..}) = case edge_code of
              D decl -> runState (transformer_decl decl) tr_st 
                -- execute the transformer
              E expr -> runState (transformer_expr expr) tr_st
      -- depending on whether the action is global or not;
      -- either add the results to the result list or update
      -- the cfg with them 
      in if isGlobal acts
         then worklist_fix tid cfgs symt cfg wlst ((st,post,acts):res)
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
           in worklist_fix tid cfgs symt cfg' nwlist res


join_update :: Map NodeId [(CState,Act)] -> NodeId -> (CState,Act) -> (Bool, Map NodeId [(CState,Act)])
join_update node_table node (st,act) =
  case M.lookup node node_table of
    Nothing -> error "join_update: not suppose to happen"
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        let nst = st `join_cstate` st'
            nact = act `join_act` act'
        in if nst == st'
           then (True, node_table)
           else (False, M.insert node [(nst,nact)] node_table)
      _ -> error "join_update: more than one state in the list"
 
strong_update :: Map NodeId [(CState,Act)] -> NodeId -> (CState,Act) -> (Bool, Map NodeId [(CState,Act)])
strong_update node_table node (st,act) =
  case M.lookup node node_table of
    Nothing -> error "strong_update: not suppose to happen"
    Just lst -> case lst of
      [] -> (False, M.insert node [(st,act)] node_table)
      [(st',act')] ->
        if st == st'
        then (True, node_table)
        else (False, M.insert node [(st,act)] node_table)
      _ -> error "strong_update: more than one state in the list" 
