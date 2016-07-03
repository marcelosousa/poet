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
import Model.GCS

instance Collapsible CState Act where
  enabled :: System CState Act -> CState -> [TId]
  enabled syst st =
    let control = controlPart st
        en = M.filter isEnabled control
    in M.keys en
  collapse :: System CState Act -> CState -> TId -> [(CState,Pos,Act)]
  collapse = undefined
  dcollapse :: System CState Act -> CState -> (TId,Pos) -> (CState,Act)
  dcollapse syst st (tid,pos) =
    let results = collapse syst st tid
        result = filter (\(s,p,a) -> p == pos) results
    in case result of
      [] -> error "dcollapse: collapse does not produce dataflow fact at desired location"
      [(st,_,act)] -> (st,act)
      _ -> error "dcollapse: collapse produced several dataflow facts for desired location"

isEnabled :: Pos -> Bool
isEnabled pos = pos >= 0 
