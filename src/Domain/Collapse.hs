{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Collapse
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- General collapse procedure:
--  Naive abstract interpretation fixpoint
--  based on a worklist algorithm
-------------------------------------------------------------------------------
module Domain.Collapse where

import qualified Data.Map as M

import Language.SimpleC.AST
import Model.GCS

gen_collapse :: Graph SymId () (st,act) -> Pos -> a 
gen_collapse cfg pos st  = undefined
         

