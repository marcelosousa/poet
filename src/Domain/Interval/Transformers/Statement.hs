{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Transformers.Statement
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Transformers for the interval semantics.
-------------------------------------------------------------------------------
module Domain.Interval.Transformers.Statement (transformer, get_addrs_expr, get_tid_expr) where

import Control.Monad.State.Lazy 
import Data.List
import Data.Map (Map)  
import Data.Maybe
import Domain.Action
import Domain.Interval.API
import Domain.Interval.State
import Domain.Interval.Transformers.State
import Domain.Interval.Transformers.Util
import Domain.Interval.Type
import Domain.Interval.Value
import Domain.MemAddr
import Domain.Util
import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Util
import Util.Generic hiding (safeLookup)
import qualified Model.GCS as GCS

-- | API for accessing the memory
-- | get_addrs computes the address of the an expression 
--   @NOTE: This is not the address in the state but the
--   symbolic address represented by the expression.
--   For example: It is entirely possible that the 
--   expr represents the address %X Global [1,5] but
--   in the state we only have information regarding %X 
--   for the offset interval [0,2].
get_addrs_expr :: IntState -> Scope -> SExpression -> IntMAddrs 
get_addrs_expr st scope expr = 
  mytrace True ("get_addrs_expr: scope = " ++ show scope ++ ", expr = " ++ show expr) $
  case expr of
    Var id -> get_addrs st $ MemAddrBase id scope 
    Unary CAdrOp e -> get_addrs_expr st scope e 
    Index lhs rhs  -> 
      let lhs_addrs = get_addrs_expr st scope lhs
          error_msg = error "get_addrs: called the transformer with missing information"
          tr_st = IntTState scope st error_msg error_msg False
          (val,_) = evalState (transformer rhs) tr_st
          -- need to filter from the lhs_addrs the interval given by val
          addrs = case lhs_addrs of 
            MemAddrTop -> MemAddrTop
            MemAddrs l -> MemAddrs $ nub $ map (flip set_offset val) l 
      in mytrace True ("get_addrs_expr: result = " ++ show addrs) $ addrs
    _ -> error $ "get_addrs_expr: expr = " ++ show expr ++ " not supported" 

-- | Get the tid associated with an expression
--   This is typically to be used in the pthread_{create, join}
get_tid_expr :: Scope -> IntState -> SExpression -> GCS.TId
get_tid_expr scope st expr = mytrace False ("get_tid_expr: " ++ show expr) $
  -- get the address(es) referenced by expr
  let th_addrs = get_addrs_expr st scope expr 
  in case read_memory st th_addrs of
    [] -> error $ "get_tid: couldnt find thread for expr " ++ show expr 
    [v] -> case v of
      IntVal [VInt tid] -> tid
      _ -> error $ "get_tid: unexpected intvalue " ++ show v 
    r -> error $ "get_tid: found too many threads for expr " ++ show expr ++ " " ++ show r 

-- | Transformer for an expression with a single state
transformer :: SExpression -> IntTOp (IntValue,IntAct)
transformer e = mytrace False ("transformer: " ++ show e) $
  case e of 
    AlignofExpr expr -> error "transformer: align_of_expr not supported"  
    AlignofType decl -> error "transformer: align_of_type not supported"
    Assign assignOp lhs rhs -> assign_transformer assignOp lhs rhs 
    Binary binaryOp lhs rhs -> binop_transformer binaryOp lhs rhs 
    BuiltinExpr built -> error "transformer: built_in_expr not supported" 
    Call fn args n -> call_transformer fn args  
    Cast decl expr -> error "transformer: cast not supported"
    Comma exprs -> error "transformer: comma not supported" 
    CompoundLit decl initList -> error "transforemr: compound literal not supported" 
    Cond cond mThenExpr elseExpr -> cond_transformer cond mThenExpr elseExpr 
    Const const -> const_transformer const 
    Index arr_expr index_expr -> index_transformer arr_expr index_expr 
    LabAddrExpr ident -> error "transformer: labaddr not supported"
    Member expr ident bool -> error "transformer: member not supported"
    SizeofExpr expr -> error "transformer: sizeof expression not supported" 
    SizeofType decl -> error "transformer: sizeof type not supported"
    Skip -> return (IntVal [],bot_act)
    StatExpr stmt -> error "transformer: stat_expr not supported"
    Unary unaryOp expr -> unop_transformer unaryOp expr 
    Var ident -> mytrace False ("calling var_trans" ++ show ident) $ var_transformer ident 
    ComplexReal expr -> error "transformer: complex op not supported" 
    ComplexImag expr -> error "transformer: complex op not supported" 

-- | Transformer for an index expression.
index_transformer :: SExpression -> SExpression -> IntTOp (IntValue, IntAct)
index_transformer lhs rhs = mytrace False ("index_transformer: lhs = " ++ show lhs ++ ", rhs = " ++ show rhs) $ do 
  s@IntTState{..} <- get
  case get_addrs_expr st scope lhs of
    MemAddrTop -> error $ "index_transformer: lhs of index operation points to MemAddrTop"
    MemAddrs l -> do
      (rhs_vals, rhs_acts) <- transformer rhs
      let l' = nub $ map (flip set_offset rhs_vals) l 
          addrs = MemAddrs l'
          vals = read_memory st addrs 
          val = join_intval_list vals
          res_acts = read_act_addr addrs `join_act` rhs_acts 
      mytrace False ("index_transformer: res_acts = " ++ show res_acts) $ return (val, res_acts) 

-- | Transformer for an assignment expression.
assign_transformer :: AssignOp -> SExpression -> SExpression -> IntTOp (IntValue,IntAct)
assign_transformer op lhs rhs = do
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_vals = case op of
        CAssignOp -> rhs_vals
        -- arithmetic operations 
        CMulAssOp -> lhs_vals * rhs_vals 
        CDivAssOp -> lhs_vals `iDivide` rhs_vals 
        CRmdAssOp -> error "mod not supported" 
        CAddAssOp -> lhs_vals + rhs_vals 
        CSubAssOp -> lhs_vals - rhs_vals
        -- bit-wise operations
        _ -> error "assign_transformer: not supported" 
  -- get the addresses of the left hand side
  s@IntTState{..} <- get
  let lhs_addrs = get_addrs_expr st scope lhs
      res_acts = add_writes lhs_addrs (rhs_acts `join_act` lhs_acts)
  -- modify the state of the addresses with
  -- the result values 
  let res_st = write_memory st lhs_addrs res_vals
  mytrace False ("assign_transformer: new state \n " ++ show res_st ) $ set_state res_st 
  return (res_vals, res_acts) 

-- | Transformer for binary operations.
--   These transformers do not change the state;
binop_transformer :: BinaryOp -> SExpression -> SExpression -> IntTOp (IntValue,IntAct)
binop_transformer binOp lhs rhs = do
  s@IntTState{..} <- get
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_vals = case binOp of
        CMulOp -> lhs_vals * rhs_vals 
        CDivOp -> lhs_vals `iDivide` rhs_vals 
        CRmdOp -> error "mod not supported for intervals" 
        CAddOp -> lhs_vals + rhs_vals 
        CSubOp -> lhs_vals - rhs_vals
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
call_transformer :: SExpression -> [SExpression] -> IntTOp (IntValue,IntAct)
call_transformer fn args =
  case fn of
    Var ident -> do
      s@IntTState{..} <- get
      let n = get_symbol_name ident sym
      call_transformer_name n args 
    _ -> error "call_transformer: not supported" 

call_transformer_name :: String -> [SExpression] -> IntTOp (IntValue,IntAct)
call_transformer_name name args = case name of
  "pthread_create" -> mytrace False ("call_transformer: pthread_create" ++ show args) $ do
    s@IntTState{..} <- get
        -- write in the address of (args !! 0) of type
        -- pthread_t, the pid of the new thread
    let (tid,s') = inc_num_th st
        th_id = get_addrs_expr s' scope (args !! 0) 
        s'' = write_memory s' th_id (IntVal [VInt tid]) 
        -- retrieve the entry point of the thread code 
        th_sym = get_expr_id $ args !! 2
        th_name = get_symbol_name th_sym sym
        (th_pos,_) = get_entry th_name i_cfgs sym
        --
        i_th_st = bot_th_state th_pos th_sym
        res_st = insert_thread s'' tid i_th_st 
    set_state res_st
    mytrace False ("STATE AFTER PTHREAD_CREATE\n" ++ show res_st) $  return (IntVal [], create_thread_act (SymId tid) zero) 
  "pthread_join" -> do
    -- this transformer is only called if it is enabled 
    s@IntTState{..} <- get
    let tid = get_tid_expr scope st (args !! 0)
    return (IntVal [], join_thread_act (SymId tid) zero) 
  "pthread_exit" -> do
    s@IntTState{..} <- get
    case scope of
      Global    -> error $ "pthread_exit: scope = Global"  
      Local tid -> return (IntVal [], exit_thread_act (SymId tid) zero)
  "pthread_mutex_lock" -> do
    -- this transformer is only called if it is enabled 
    s@IntTState{..} <- get
    let mutex_addr = get_addrs_expr st scope (args !! 0)
        res_st = write_memory st mutex_addr one 
    set_state res_st
    return (one, lock_act_addr mutex_addr) 
  "pthread_mutex_unlock" -> do
    -- this transformer is only called if it is enabled 
    s@IntTState{..} <- get
    let mutex_addr = get_addrs_expr st scope (args !! 0)
        res_st = write_memory st mutex_addr zero 
    set_state res_st
    return (zero, unlock_act_addr mutex_addr) 
  "pthread_mutex_init" -> do
    -- this transformer is only called if it is enabled 
    s@IntTState{..} <- get
    let mutex_addr = get_addrs_expr st scope (args !! 0)
        res_st = write_memory st mutex_addr zero 
    set_state res_st
    return (zero, write_act_addr mutex_addr) 
  "nondet" -> do 
    (lVal,lacts) <- transformer $ args !! 0
    (uVal,uacts) <- transformer $ args !! 1
    case (lVal,uVal) of 
      (IntVal [VInt l],IntVal [VInt u]) -> 
       return (InterVal (I l, I u),lacts `join_act` uacts)
      (l, u) -> 
       return (l `iJoin` u,lacts `join_act` uacts)
  "poet_error" -> error "poet_error: assertion is violated" 
  _ -> mytrace False ("call_transformer_name: calls to " ++ name ++ " being ignored") $
     return (zero, bot_act) 

-- Need to apply the cut over the state
-- for the cond + then expression
-- for the not cond + else expression
-- and join both states
cond_transformer :: SExpression -> Maybe SExpression -> SExpression -> IntTOp (IntValue,IntAct)
cond_transformer cond mThen else_e = error "cond_transformer not supported"
{- do
  s <- get
  -- Then part
  cond_acts <- bool_transformer_expr cond
  sCond <- get
  (tState,tVal,t_) <- if is_bot sCond
            then sCond -- bottom 
            else case mThen of
              Nothing -> error "cond_transformer: cond is true and there is not then"
              Just e  -> transformer e >>= \(vt,at) -> get >>= \s -> return (s,vt,at)
  -- Else part
  put s
  ncond_acts <- bool_transformer_expr $ Unary CNegOp cond
  sElse <- get
  eState <- if is_bot sElse
            then sElse
            else transformer else_e >> get
  let ns = tState `join_intstate` eState
  put ns
-}  
{-
do
  (vals,acts) <- transformer cond
  -- vals is both false and true 
  let (isTrue,isFalse) = checkBoolVals vals
  (tVal,tIntAct) <-
    if isTrue
    then case mThen of
           Nothing -> error "cond_transformer: cond is true and there is not then"
           Just e -> transformer e
    else return ([],bot_act)
  (eVal,eIntAct) <-
    if isFalse
    then transformer else_e
    else return ([],bot_act)
  let res_vals = tVal ++ eVal
      res_acts = acts `join_act` tIntAct `join_act` eIntAct
  return (res_vals,res_acts) 
-}

-- | Transformer for constants.
const_transformer :: Constant -> IntTOp (IntValue,IntAct)
const_transformer const =
  case toValue const of
    VInt i -> return (InterVal (I i, I i), bot_act)
    _ -> error "const_transformer: not supported non-int constants"

-- | Transformer for unary operations.
unop_transformer :: UnaryOp -> SExpression -> IntTOp (IntValue,IntAct)
unop_transformer unOp expr = do 
  case unOp of
    CPreIncOp  -> error "unop_transformer: CPreIncOp  not supported"     
    CPreDecOp  -> error "unop_transformer: CPreDecOp  not supported"  
    CPostIncOp -> transformer $ Assign CAddAssOp expr (Const const_one)  
    CPostDecOp -> error "unop_transformer: CPostDecOp not supported"   
    CAdrOp     -> error "unop_transformer: CAdrOp     not supported"    
    CIndOp     -> error "unop_transformer: CIndOp     not supported" 
    CPlusOp    -> transformer expr 
    CMinOp     -> do
      (expr_vals,res_acts) <- transformer expr 
      return ((InterVal (I 0, I 0)) - expr_vals,res_acts)
    CCompOp    -> error "unop_transformer: CCompOp not supported" 
    CNegOp     -> transformer $ negExp expr 

-- | Transformer for var expressions.
var_transformer :: SymId -> IntTOp (IntValue, IntAct)
var_transformer sym_id = mytrace False ("var_transformer: sym_id = " ++ show sym_id) $ do
  s@IntTState{..} <- get
  let id_addr_base = MemAddrBase sym_id scope
      id_addrs = get_addrs st id_addr_base 
      vals = read_memory st id_addrs 
      val = join_intval_list vals 
      acts = read_act_addr id_addrs 
  return (val, acts)
