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

import qualified Data.Map as M

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
gen_collapse :: TId -> ConGraphs -> SymbolTable -> ConGraph -> Pos -> CState -> [(CState,Pos,Act)]
gen_collapse tid cfgs symt cfg@Graph{..} pos st =
  -- reset the node table information with the information passed
  let node_table' = M.insert pos [(st,bot_act)] $ M.map (const []) node_table
      wlist = map (\(a,b) -> (pos,a,b)) $ succs cfg pos
  in worklist_fix tid cfgs symt cfg wlist st

worklist_fix :: TId -> ConGraphs -> SymbolTable -> ConGraph -> Worklist -> CState -> [(CState,Pos,Act)]
worklist_fix tid cfgs symt cfg@Graph{..} wlist st =
  case wlist of
    [] -> undefined
    ((pre,eId,post):wlst) ->
      let e@EdgeInfo{..} = get_edge_info cfg eId
      in undefined 
 
  -- construct the transformer state
  -- let tr_st = ConTState (Local tid) st bot_sigma symt cfgs 
