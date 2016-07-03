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
  -- collapse :: System CState Act -> CState -> TId -> [(CState,Pos,Act)]
  collapse = undefined

