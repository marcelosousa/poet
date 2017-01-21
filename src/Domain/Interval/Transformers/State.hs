{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Transformers.State
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Transformers for the interval semantics.
-------------------------------------------------------------------------------
module Domain.Interval.Transformers.State where

import Control.Monad.State.Lazy 
import Domain.Util
import Domain.Interval.State
import Domain.Interval.Value
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Flow
import Language.SimpleC.Converter hiding (Scope(..))
import Data.Map

-- | State of the abstract transformer
data IntTState 
 = IntTState 
 {
   scope :: Scope
 , st :: IntState          -- the state
 , sym :: Map SymId Symbol
 , i_cfgs :: Graphs SymId () (IntState, IntAct)
 , cond :: Bool            -- is a condition? 
 , node_id :: Int 
 }

-- | Transformer operation 
type IntTOp val = State IntTState val

set_state :: IntState -> IntTOp ()
set_state nst = do
  s@IntTState{..} <- get
  put s { st = nst }

