{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Transformers.System
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- Transformers for the concrete semantics.
--  Transformer for the system
-------------------------------------------------------------------------------
module Domain.Concrete.Transformers.System where

import Control.Monad.State.Lazy 
import Data.List 
import Data.Map (Map)  
import Data.Maybe
import Domain.Action
import Domain.Interval.API (set_pos)
import Domain.Interval.State
import Domain.Interval.Transformers.State
import Domain.Interval.Transformers.Declaration
import Domain.Interval.Transformers.Util
import Domain.Interval.Type
import Domain.Interval.Value
import Domain.MemAddr
import Domain.Util
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Flow
import Language.SimpleC.Util
import Util.Generic hiding (safeLookup)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Model.GCS as GCS

-- | converts the front end into a system
convert :: FrontEnd () (IntState,IntAct) -> GCS.System IntState IntAct
convert fe = 
  let (pos_main,sym_main) = get_entry "main" (cfgs fe) (symt fe)
      init_tstate = IntTState Global empty_state (symt fe) (cfgs fe) False 0 S.empty
      (acts,s@IntTState{..}) = runState (transformer_decls $ decls $ ast fe) init_tstate
      st' = set_pos st (symId sym_main) sym_main pos_main  
  in mytrace False ("convert: initial state = " ++ show st) $ GCS.System st' acts (cfgs fe) (symt fe) [GCS.main_tid] 1

-- | processes a list of declarations 
transformer_decls :: [SDeclaration] -> IntTOp IntAct
transformer_decls = mytrace False ("transformer_decls!!!!") $
  foldM (\a d -> transformer_decl d >>= \a' -> return $ join_act a a') bot_act 
