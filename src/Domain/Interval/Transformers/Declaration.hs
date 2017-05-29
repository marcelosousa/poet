{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Transformers.Declaration
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Transformers for the interval semantics.
--   transformer_decl (for the declaration)
-------------------------------------------------------------------------------
module Domain.Interval.Transformers.Declaration (transformer_decl) where

import Control.Monad.State.Lazy hiding (join)
import Data.List 
import Data.Map (Map)  
import Data.Maybe
import Domain.Action
import Domain.Interval.API (write_memory_addr)
import Domain.Interval.State
import Domain.Interval.Transformers.State
import Domain.Interval.Transformers.Statement
import Domain.Interval.Type
import Domain.Interval.Value
import Domain.Lattice
import Domain.MemAddr
import Domain.Util
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Flow
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)
import qualified Data.Map as M
import qualified Data.Set as S


-- | transformer for a declaration:
transformer_decl :: SDeclaration -> IntTOp IntAct
transformer_decl decl = mytrace False ("transformer_decl: " ++ show decl) $ do
  case decl of
    TypeDecl ty -> return bot -- error "transformer_decl: not supported yet"
    Decl ty el@DeclElem{..} ->
      case declarator of
        Nothing -> 
          case initializer of 
            Nothing -> return bot
            _ -> error "initializer w/ declarator" 
        Just d@Declr{..} ->
          case declr_ident of
            Nothing -> error "no identifier" 
            Just id ->   
              let typ = Ty declr_type ty
              in  transformer_init id typ initializer

-- | processes a declaration initializer 
--   for the symbol id with type ty.
--   This is different than an assignment because
--   the position for this id is empty in the state
--   so any lookup would fail.
--   This function needs to receive a state and can 
--   potentially create several states. 
transformer_init :: SymId -> STy -> Maybe (Initializer SymId ()) -> IntTOp IntAct
transformer_init id ty minit = mytrace False ("transformer_init for " ++ show id ++ " with init " ++ show minit ++ ", ty = " ++ show ty) $ do
  case minit of
    Nothing -> do
      s@IntTState{..} <- get
      (vals, i_acts) <- default_value ty
      let id_addrs_vals = map (\(off,val) -> (MemAddr id (kVal off) scope, val)) $ zip [0..] vals 
          st' = foldr (\(addr,val) _st -> write_memory_addr _st addr val) st id_addrs_vals
          acts = write_act_addr $ MemAddrs $ fst $ unzip id_addrs_vals 
      set_state st'
      return $ acts `join` i_acts
    Just i  -> case i of
      InitExpr expr -> do
        -- for each state, we need to apply the transformer
        s@IntTState{..} <- get
        (val, acts) <- transformer expr
        let id_addr = MemAddr id zero scope
            st' = write_memory_addr st id_addr val 
            acts' = add_writes (MemAddrs [id_addr]) acts
        set_state st'
        mytrace False ("transformer_init: setting state = " ++ show st') $ return acts'
      InitList list -> error "transformer_init: initializer list is not supported"

-- | Default value of a type
--   If we are given a base type, then we
--   simply generate a default value.
--   @NOTE V1: No support for pointer, arrays or structs.
--    This means that it is simply calling the default
--    initializer from the simplec package
--   If it is a pointer type, then we
--   generate a VPtr 0 (denoting NULL).
--   If it is an array type ?
--   If it is a struct type ? 
default_value :: Ty SymId () -> IntTOp ([IntValue], IntAct)
default_value (Ty declarators ty) =
  case declarators of
   [ArrDeclr t (ArrSize b size_expr)] -> do
     (val, act) <- transformer size_expr
     case val of
       InterVal (I n, I m) ->
         if n == m
         then return (replicate n zero, bot) 
         else error "default_value: unsupported interval for size expression" 
       _ -> error "default_value: unsupported value for size expression"  
   _ -> return ([zero], bot)
