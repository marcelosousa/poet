-------------------------------------------------------------------------------
-- Module    :  Domain.Synchro
-- Copyright :  (c) 2015 Marcelo Sousa
-- 
-- Modules for the synchronisation semantics
-------------------------------------------------------------------------------
module Domain.Synchro where

-- import Domain.Synchro.Collapse
-- import Domain.Synchro.Converter
-- import Domain.Synchro.State

type TId = Integer
type Pos = Integer
data System = System
data Act = Act
  deriving (Show, Eq, Ord)
