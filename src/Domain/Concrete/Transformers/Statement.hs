{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Transformers.Statement
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- Transformers for the concrete semantics.
-------------------------------------------------------------------------------
module Domain.Concrete.Transformers.Statement (transformer, get_addrs_expr, get_tid_expr, has_exited, is_locked) where

import Control.Monad.State.Lazy 
import Data.List
import Data.Map (Map)  
import Data.Maybe
import Domain.Action
import Domain.Concrete.API
import Domain.Concrete.State
import Domain.Concrete.Transformers.State
import Domain.Concrete.Value
import Domain.MemAddr
import Domain.Util
import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import qualified Language.SimpleC.Flow as F
import Language.SimpleC.Util
import Util.Generic hiding (safeLookup)
import qualified Model.GCS as GCS
import qualified Data.Map as M 
import qualified Data.Set as S 

-- | Transformer for an expression with a single state
transformer :: SExpression -> ConTOp (ConValue,ConAct)
transformer e = mytrace False ("transformer: " ++ show e) $
  case e of 
    AlignofExpr expr -> error "transformer: align_of_expr not supported"  
    AlignofType decl -> error "transformer: align_of_type not supported"
    Assign assignOp lhs rhs -> assign_transformer assignOp lhs rhs 
    Binary binaryOp lhs rhs -> binop_transformer binaryOp lhs rhs 
    BuiltinExpr built -> error "transformer: built_in_expr not supported" 
    Call fn args n -> call_transformer fn args  
    Cast decl expr -> return (zero, bot_act)
    Comma exprs -> error "transformer: comma not supported" 
    CompoundLit decl initList -> error "transforemr: compound literal not supported" 
    Cond cond mThenExpr elseExpr -> cond_transformer cond mThenExpr elseExpr 
    Const const -> const_transformer const 
    Index arr_expr index_expr -> index_transformer arr_expr index_expr 
    LabAddrExpr ident -> error "transformer: labaddr not supported"
    Member expr ident bool -> error "transformer: member not supported"
    SizeofExpr expr -> error "transformer: sizeof expression not supported" 
    SizeofType decl -> error "transformer: sizeof type not supported"
    Skip -> return (zero, bot_act)
    StatExpr stmt -> error "transformer: stat_expr not supported"
    Unary unaryOp expr -> unop_transformer unaryOp expr 
    Var ident -> mytrace False ("calling var_trans" ++ show ident) $ var_transformer ident 
    ComplexReal expr -> error "transformer: complex op not supported" 
    ComplexImag expr -> error "transformer: complex op not supported" 

-- | Transformer for an index expression.
index_transformer :: SExpression -> SExpression -> ConTOp (ConValue, ConAct)
index_transformer lhs rhs = mytrace False ("index_transformer: lhs = " ++ show lhs ++ ", rhs = " ++ show rhs) $ do 
  s@ConTState{..} <- get
  case get_addrs_expr st scope lhs of
    MemAddrTop -> error $ "index_transformer: lhs of index operation points to MemAddrTop"
    MemAddrs l -> do
      (rhs_vals, rhs_acts) <- transformer rhs
      let l' = nub $ map (flip set_offset rhs_vals) l 
          addrs = MemAddrs l'
          vals = read_memory st addrs 
          val = case vals of
             [v] -> v
             x   -> error "index_transformer: error"
          res_acts = read_act_addr addrs `join_act` rhs_acts 
      mytrace False ("index_transformer: res_acts = " ++ show res_acts) $ return (val, res_acts) 

-- | Transformer for an assignment expression.
assign_transformer :: AssignOp -> SExpression -> SExpression -> ConTOp (ConValue,ConAct)
assign_transformer op lhs rhs = do
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_vals = case op of
        CAssignOp -> rhs_vals
        -- arithmetic operations 
        CMulAssOp -> lhs_vals `mult_conval` rhs_vals 
        CDivAssOp -> lhs_vals `divs_conval` rhs_vals 
        CRmdAssOp -> lhs_vals `rmdr_conval` rhs_vals 
        CAddAssOp -> lhs_vals `add_conval`  rhs_vals 
        CSubAssOp -> lhs_vals `sub_conval`  rhs_vals
        -- bit-wise operations
        _ -> error "assign_transformer: not supported" 
  -- get the addresses of the left hand side
  s@ConTState{..} <- get
  let lhs_addrs = get_addrs_expr st scope lhs
      res_acts = add_writes lhs_addrs (rhs_acts `join_act` lhs_acts)
  -- modify the state of the addresses with
  -- the result values 
  let res_st = write_memory st lhs_addrs res_vals
  mytrace False ("assign_transformer: new state \n " ++ show res_st ) $ set_state res_st 
  return (res_vals, res_acts) 

cond_transformer :: SExpression -> Maybe SExpression -> SExpression -> ConTOp (ConValue,ConAct)
cond_transformer cond mThen else_e = error "cond_transformer not supported"

-- | Transformer for binary operations.
--   These transformers do not change the state;
binop_transformer :: BinaryOp -> SExpression -> SExpression -> ConTOp (ConValue,ConAct)
binop_transformer binOp lhs rhs = do
  s@ConTState{..} <- get
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_vals = case binOp of
        CMulOp -> lhs_vals `mult_conval` rhs_vals
        CDivOp -> lhs_vals `divs_conval` rhs_vals
        CRmdOp -> lhs_vals `rmdr_conval` rhs_vals
        CAddOp -> lhs_vals `add_conval`  rhs_vals 
        CSubOp -> lhs_vals `sub_conval`  rhs_vals
        -- boolean operations 
        -- bit-wise operations 
        CShlOp -> error "binop_transformer: CShlOp not supported"  
        CShrOp -> error "binop_transformer: CShrOp not supported" 
        CXorOp -> error "binop_transformer: CXorOp not supported" 
        CLndOp -> error "binop_transformer: CLndOp not supported" 
        CLorOp -> error "binop_transformer: CLorOp not supported"
        _ -> error "binop_transtranformer: not completed" -- apply_logic binOp lhs rhs
      res_acts = lhs_acts `join_act` rhs_acts
  return (res_vals,res_acts)

-- | Transformer for call expression:
--   Support for pthread API functions
--   pthread_create
--   pthread_join
--   lock
--   unlock  
call_transformer :: SExpression -> [SExpression] -> ConTOp (ConValue,ConAct)
call_transformer fn args = mytrace False ("call_transformer: " ++ show fn ++ " " ++ show args) $ 
  case fn of
    Var ident -> do
      s@ConTState{..} <- get
      let n = get_symbol_name ident sym
      call_transformer_name n args 
    _ -> error "call_transformer: not supported" 

-- has_exited makes the assumptions that there is no sucessors of an exit node
-- wrong for more complex CFGs
has_exited :: F.Graphs SymId () (ConState, ConAct) -> ConState -> GCS.TId -> Bool
has_exited _cfgs st tid = 
  let control = GCS.controlPart st
      tid_cfg_sym = GCS.toThCFGSym st tid
  in case M.lookup tid control of
       Nothing  -> False
       Just pos -> case M.lookup tid_cfg_sym _cfgs of 
         Nothing  -> error $ "has_exited fatal: tid " ++ show tid ++ " not found in cfgs"
         Just cfg -> case F.succs cfg pos of
           [] -> True
           _ -> False 

is_locked :: ConState -> Scope -> SExpression -> Bool
is_locked st scope expr = mytrace False ("is_locked: scope = " ++ show scope ++ ", expr = " ++ show expr) $ 
  let lk_addrs = get_addrs_expr st scope expr 
  in case read_memory st lk_addrs of
    []    -> error $ "is_locked fatal: cant find info for lock " ++ show expr
    [val] -> val == one 
    l -> error $ "is_locked fatal: lock has unsupported values " ++ show l 

call_transformer_name :: String -> [SExpression] -> ConTOp (ConValue,ConAct)
call_transformer_name name args = case name of
  "pthread_create" -> mytrace False ("call_transformer: pthread_create" ++ show args) $ do
    s@ConTState{..} <- get
        -- write in the address of (args !! 0) of type
        -- pthread_t, the pid of the new thread
    let (tid,s') = inc_num_th st
        th_id = get_addrs_expr s' scope (args !! 0) 
        s'' = write_memory s' th_id (ConVal (VInt tid)) 
        -- retrieve the entry point of the thread code 
        th_sym = get_expr_id $ args !! 2
        th_name = get_symbol_name th_sym sym
        (th_pos,_) = get_entry th_name cfgst sym
        --
        i_th_st = bot_th_state th_pos th_sym
        res_st = insert_thread s'' tid i_th_st 
    set_state res_st
    mytrace False ("STATE AFTER PTHREAD_CREATE\n" ++ show res_st) $  return (zero, create_thread_act (SymId tid) zero) 
  "pthread_join" -> do
    -- if the transformer is not enabled it returns the bottom state
    s@ConTState{..} <- get
    let tid = get_tid_expr scope st (args !! 0)
    if has_exited cfgst st tid
    then return (zero, join_thread_act (SymId tid) zero) 
    else do
      set_state $ set_cstate_bot st
      return (zero, bot_act) 
  "pthread_exit" -> do
    s@ConTState{..} <- get
    case scope of
      Global    -> error $ "pthread_exit: scope = Global"  
      Local tid -> return (zero, exit_thread_act (SymId tid) zero)
  "pthread_mutex_lock" -> do
    -- if the transformer is not enabled it returns the bottom state
    s@ConTState{..} <- get
    if is_locked st scope (args !! 0)
    then do
      set_state $ set_cstate_bot st
      return (zero, bot_act)
    else do 
      let mutex_addr = get_addrs_expr st scope (args !! 0)
          res_st = write_memory st mutex_addr one 
      set_state res_st
      return (one, lock_act_addr mutex_addr) 
  "pthread_mutex_unlock" -> do
    -- this transformer is only called if it is enabled 
    s@ConTState{..} <- get
    let mutex_addr = get_addrs_expr st scope (args !! 0)
        res_st = write_memory st mutex_addr zero 
    set_state res_st
    return (zero, unlock_act_addr mutex_addr) 
  "pthread_mutex_init" -> do
    -- this transformer is only called if it is enabled 
    s@ConTState{..} <- get
    let mutex_addr = get_addrs_expr st scope (args !! 0)
        res_st = write_memory st mutex_addr zero 
    set_state res_st
    return (zero, write_act_addr mutex_addr) 
  "__VERIFIER_nondet_int" -> error "NONDET not supporte in concrete sem"
  "__VERIFIER_error" -> do
    s@ConTState{..} <- get
    add_warn node_id
    mytrace False ("__VERIFIER_error: position = " ++ show node_id) $ return (zero, bot_act)
  _ -> mytrace False ("call_transformer_name: calls to " ++ name ++ " being ignored") $
     return (zero, bot_act) 

-- | Transformer for constants.
const_transformer :: Constant -> ConTOp (ConValue,ConAct)
const_transformer k = return (ConVal (toValue k), bot_act)

-- | Transformer for unary operations.
unop_transformer :: UnaryOp -> SExpression -> ConTOp (ConValue,ConAct)
unop_transformer unOp expr = do 
  case unOp of
    CPreIncOp  -> error "unop_transformer: CPreIncOp  not supported"     
    CPreDecOp  -> error "unop_transformer: CPreDecOp  not supported"  
    CPostIncOp -> transformer $ Assign CAddAssOp expr (Const const_one)  
    CPostDecOp -> transformer $ Assign CSubAssOp expr (Const const_one)
    CAdrOp     -> error "unop_transformer: CAdrOp     not supported"    
    CIndOp     -> error "unop_transformer: CIndOp     not supported" 
    CPlusOp    -> transformer expr 
    CMinOp     -> do
      (expr_vals,res_acts) <- transformer expr 
      return (minus_conval expr_vals,res_acts)
    CCompOp    -> error "unop_transformer: CCompOp not supported" 
    CNegOp     -> transformer $ negExp expr 

-- | Transformer for var expressions.
var_transformer :: SymId -> ConTOp (ConValue, ConAct)
var_transformer sym_id = mytrace False ("var_transformer: sym_id = " ++ show sym_id) $ do
  s@ConTState{..} <- get
  let id_addr_base = MemAddrBase sym_id scope
      id_addrs = get_addrs st id_addr_base 
      vals = read_memory st id_addrs 
      val = case vals of
             [v] -> v
             x   -> error "var_transformer: error"
      acts = read_act_addr id_addrs 
  return (val, acts)
  
-- | API for accessing the memory
-- | get_addrs computes the address of the an expression 
--   @NOTE: This is not the address in the state but the
--   symbolic address represented by the expression.
--   For example: It is entirely possible that the 
--   expr represents the address %X Global [1,5] but
--   in the state we only have information regarding %X 
--   for the offset interval [0,2].
get_addrs_expr :: ConState -> Scope -> SExpression -> ConMAddrs 
get_addrs_expr st scope expr = 
  mytrace False ("get_addrs_expr: scope = " ++ show scope ++ ", expr = " ++ show expr) $
  case expr of
    Var id -> get_addrs st $ MemAddrBase id scope 
    Unary CAdrOp e -> get_addrs_expr st scope e 
    Index lhs rhs  -> 
      let lhs_addrs = get_addrs_expr st scope lhs
          error_msg = error "get_addrs: called the transformer with missing information"
          tr_st = ConTState scope st error_msg error_msg False False 0 S.empty
          (val,_) = evalState (transformer rhs) tr_st
          -- need to filter from the lhs_addrs the interval given by val
          addrs = case lhs_addrs of 
            MemAddrTop -> MemAddrTop
            MemAddrs l -> MemAddrs $ nub $ map (flip set_offset val) l 
      in mytrace False ("get_addrs_expr: result = " ++ show addrs) $ addrs
    _ -> error $ "get_addrs_expr: expr = " ++ show expr ++ " not supported" 

-- | Get the tid associated with an expression
--   This is typically to be used in the pthread_{create, join}
get_tid_expr :: Scope -> ConState -> SExpression -> GCS.TId
get_tid_expr scope st expr = mytrace False ("get_tid_expr: " ++ show expr) $
  -- get the address(es) referenced by expr
  let th_addrs = get_addrs_expr st scope expr 
  in case read_memory st th_addrs of
    [] -> error $ "get_tid: couldnt find thread for expr " ++ show expr 
    [v] -> case v of
      ConVal (VInt tid) -> tid
      _ -> error $ "get_tid: unexpected intvalue " ++ show v 
    r -> error $ "get_tid: found too many threads for expr " ++ show expr ++ " " ++ show r 

