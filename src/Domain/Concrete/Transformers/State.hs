-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Transformers.State
-- Copyright :  (c) 2015-17 Marcelo Sousa
--
-------------------------------------------------------------------------------
module Domain.Concrete.Transformers.State where

import Control.Monad.State.Lazy 
import Domain.Util
import Domain.Concrete.State
import Domain.Concrete.Value
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Flow
import Language.SimpleC.Converter hiding (Scope(..))
import Data.Map
import Data.Set (Set)
import qualified Data.Set as S

-- | State of the concrete transformer
data ConTState 
 = ConTState 
 {
   scope :: Scope
 , st    :: ConState         -- the set of states
 , sym   :: Map SymId Symbol
 , cfgst :: Graphs SymId () (ConState, ConAct)
 , cond  :: Bool            -- is a condition? 
 , exit  :: Bool            -- is an exit? 
 , warns :: Set Int
 }

-- | Transformer operation 
type ConTOp val = State ConTState val

-- | Set the state
set_state :: ConState -> ConTOp ()
set_state nst = do
  s@ConTState{..} <- get
  put s { st = nst }

-- | Joins with input state 
join_state :: ConState -> ConTOp ()
join_state nst = do
  s@ConTState{..} <- get
  let fst = nst `join_cstate` st
  put s { st = fst }

-- | Adds a warning to the state
add_warn :: Int -> ConTOp ()
add_warn n = do 
  s@ConTState{..} <- get
  let _warns = S.insert n warns
  put s { warns = _warns }

