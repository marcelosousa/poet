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
import Model.GCS

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
    in gen_collapse th_cfg pos st

gen_collapse :: Graph SymId () (CState,Act) -> Pos -> CState -> [(CState,Pos,Act)]
gen_collapse = undefined 
