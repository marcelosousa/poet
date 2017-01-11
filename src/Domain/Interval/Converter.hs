{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Converter
-- Copyright :  (c) 2016 Marcelo Sousa
--
-- Transformers for the interval semantics.
-- Two main transformers for the types of edges in the CFG:
--   transformer_decl (for the declaration)
--   transformer_expr (for the expression) 
-------------------------------------------------------------------------------
module Domain.Interval.Converter where

import Control.Monad.State.Lazy 
import Data.List 
import Data.Map (Map)  
import Data.Maybe
import Domain.Action
import Domain.Interval.API
import Domain.Interval.State
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

-- | State of the abstract transformer
data IntTState 
 = IntTState 
 {
   scope :: Scope
 , st :: IntState          -- the state
 , sym :: Map SymId Symbol
 , i_cfgs :: Graphs SymId () (IntState, IntAct)
 , cond :: Bool            -- is a condition? 
 }

-- | Transformer operation 
type IntTOp val = State IntTState val

set_state :: IntState -> IntTOp ()
set_state nst = do
  s@IntTState{..} <- get
  put s { st = nst }

-- | API for accessing the memory
-- | get_addrs retrieves the information from the points to analysis.
get_addrs_expr :: IntState -> Scope -> SExpression -> IntMAddrs 
get_addrs_expr st scope expr = 
  mytrace False ("get_addrs_expr: scope = " ++ show scope ++ ", expr = " ++ show expr) $
  case expr of
    Var id -> get_addrs st $ MemAddrBase id scope 
    Unary CAdrOp e -> get_addrs_expr st scope e 
    Index lhs rhs  -> 
      let lhs_addrs = get_addrs_expr st scope lhs
          error_msg = error "get_addrs: called the transformer with missing information"
          tr_st = IntTState scope st error_msg error_msg False
          (val,_) = evalState (transformer rhs) tr_st
          -- need to filter from the lhs_addrs the interval given by val
          addrs = undefined -- map (flip set_offset val) l 
      in addrs
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

-- | converts the front end into a system
-- @REVISED: July'16
convert :: FrontEnd () (IntState,IntAct) -> GCS.System IntState IntAct
convert fe = 
  let (pos_main,sym_main) = get_entry "main" (cfgs fe) (symt fe)
      init_tstate = IntTState Global empty_state (symt fe) (cfgs fe) False
      (acts,s@IntTState{..}) = runState (transformer_decls $ decls $ ast fe) init_tstate
      st' = set_pos st (symId sym_main) sym_main pos_main  
  in trace ("convert: initial state = " ++ show st) $ GCS.System st' acts (cfgs fe) (symt fe) [GCS.main_tid] 1

-- | retrieves the entry node of the cfg of a function
get_entry :: String -> Graphs SymId () (IntState, IntAct) -> Map SymId Symbol -> (GCS.Pos, SymId)
get_entry fnname graphs symt = 
  case M.foldrWithKey aux_get_entry Nothing graphs of
    Nothing -> error $ "get_entry: cant get entry for " ++ fnname
    Just p -> p -- T.trace (show symt) p
 where 
   aux_get_entry sym cfg r = 
     case r of 
       Just res -> Just res
       Nothing -> 
         case M.lookup sym symt of
           Nothing -> Nothing
           Just s  -> if get_name s == fnname
                      then Just (entry_node cfg, sym)
                      else Nothing 

-- | processes a list of declarations 
transformer_decls :: [SDeclaration] -> IntTOp IntAct
transformer_decls = mytrace False ("transformer_decls!!!!") $
  foldM (\a d -> transformer_decl d >>= \a' -> return $ join_act a a') bot_act 

-- | transformer for a declaration:
transformer_decl :: SDeclaration -> IntTOp IntAct
transformer_decl decl = mytrace False ("transformer_decl: " ++ show decl) $ do
  case decl of
    TypeDecl ty -> error "transformer_decl: not supported yet"
    Decl ty el@DeclElem{..} ->
      case declarator of
        Nothing -> 
          case initializer of 
            Nothing -> return bot_act
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
      let id_addrs_vals = map (\(off,val) -> (MemAddr id (k off) scope, val)) $ zip [0..] vals 
          st' = case scope of 
            Global -> foldr (\(addr,val) _st -> insert_heap _st addr ty val) st id_addrs_vals 
            Local i->foldr (\(addr,val) _st -> write_memory_addr st addr val) st id_addrs_vals 
          acts = write_act_addr $ MemAddrs $ fst $ unzip id_addrs_vals 
      set_state st'
      return $ acts `join_act` i_acts
    Just i  -> case i of
      InitExpr expr -> do
        -- for each state, we need to apply the transformer
        s@IntTState{..} <- get
        (val, acts) <- transformer expr
        let id_addr = MemAddr id zero scope
            st' = case scope of 
              Global -> insert_heap st id_addr ty val 
              Local i -> write_memory_addr st id_addr val 
            acts' = add_writes (MemAddrs [id_addr]) acts
        set_state st'
        trace ("transformer_init: setting state = " ++ show st') $ return acts'
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
         then return (replicate n zero, bot_act) 
         else error "default_value: unsupported interval for size expression" 
       _ -> error "default_value: unsupported value for size expression"  
   _ -> return ([zero], bot_act)

-- | Transformers for interval semantics 
-- Given an initial state and an expression
-- return the updated state.
transformer_expr :: SExpression -> IntTOp IntAct
transformer_expr expr = mytrace False ("transformer_expr: " ++ show expr) $ do
  s@IntTState{..} <- get
  if cond
  then trace ("transformer_expr: conditional " ++ show expr) $ do 
    (val, act) <- bool_transformer_expr expr
    s@IntTState{..} <- get
    let res_st = case val of
          IntBot -> set_int_state_bot st
          _ -> st
    set_state res_st 
    trace ("bool_transformer: result = " ++ show val) $ return act
  else do
    (vals,act) <- transformer expr
    return act 
    
-- eval logical expressions
-- we are going to be conservative;
-- all the variables in this expression
-- are going to be considered written.
-- We follow the implementation described
-- in https://www.di.ens.fr/~rival/semverif-2016/sem-11-ai.pdf
-- starting from page 28.
-- Only <=, \/ and /\ will be implemented.
bool_transformer_expr :: SExpression -> IntTOp (IntValue, IntAct)
bool_transformer_expr expr = case expr of
  Binary op lhs rhs -> apply_logic op lhs rhs
  Unary CNegOp rhs ->
    let rhs' = negExp rhs
    in bool_transformer_expr rhs' 
  _ -> error $ "bool_transformer_expr: not supported " ++ show expr

apply_logic :: BinaryOp -> SExpression -> SExpression -> IntTOp (IntValue,IntAct)
apply_logic op lhs rhs =
  let one = Const $ IntConst $ CInteger 1 DecRepr $ Flags 0 
  in case op of
    -- e1 < e2 ~> e1 <= (e2 - 1)
    CLeOp  -> interval_leq lhs (Binary CSubOp rhs one)
    -- e1 > e2 ~> e2 <= (e1 - 1)
    CGrOp  -> interval_leq (Binary CAddOp rhs one) lhs
    -- e1 <= e2
    CLeqOp -> interval_leq lhs rhs
    -- e1 >= e2 ~> e2 <= e1
    CGeqOp -> interval_leq rhs lhs
    -- e1 == e2 ~> (e1 <= e2) and (e2 <= e1)
    CEqOp  ->
      let lhs' = Binary CLeqOp lhs rhs
          rhs' = Binary CLeqOp rhs lhs
      in apply_logic CLndOp lhs' rhs'
    -- e1 != e2 ~> (e1 <= (e2 - 1)) or (e2 <= (e1 - 1))
    CNeqOp ->
      let lhs' = Binary CLeqOp lhs (Binary CSubOp rhs one)
          rhs' = Binary CLeqOp rhs (Binary CSubOp lhs one)
      in apply_logic CLorOp lhs' rhs'
    CLndOp -> do
      (lhs_val, lhs_act) <- bool_transformer_expr lhs 
      (rhs_val, rhs_act) <- bool_transformer_expr rhs
      let acts = lhs_act `join_act` rhs_act
      if lhs_val /= IntBot && rhs_val /= IntBot
      then return (lhs_val, acts)
      else return (IntBot, acts)
    CLorOp -> do
      (lhs_val, lhs_act) <- bool_transformer_expr lhs 
      (rhs_val, rhs_act) <- bool_transformer_expr rhs
      let res_val = if lhs_val /= IntBot 
                    then lhs_val
                    else if rhs_val /= IntBot
                         then rhs_val
                         else IntBot
      return (res_val, lhs_act `join_act` rhs_act)

-- Logical Operations
-- Need to update the variables
interval_leq :: SExpression -> SExpression -> IntTOp (IntValue, IntAct)
interval_leq lhs rhs = trace ("inter_leq: lhs = " ++ show lhs ++ ", rhs = " ++ show rhs) $ do
  (lhs_val, lhs_act) <- transformer lhs 
  (rhs_val, rhs_act) <- transformer rhs 
  let acts = lhs_act `join_act` rhs_act
      (a,b) = (lowerBound lhs_val, upperBound lhs_val)
      (c,d) = (lowerBound rhs_val, upperBound rhs_val)
  if lhs_val == IntBot || rhs_val == IntBot || a > d
  then return (IntBot, acts)
  else do
    s@IntTState{..} <- get
    -- Update the variables if we can 
    let lhs_nval = InterVal (a, min b d)
        rhs_nval = InterVal (max a c, d) 
        (lhs_st, lhs_nact) = 
          case get_addrs st scope lhs of
            Nothing -> (st, bot_act)
            Just lhs_addr -> (write_memory st lhs_addr lhs_nval, write_act_addr lhs_addr)
        (rhs_st, rhs_nact) = 
          case get_addrs lhs_st scope rhs of
            Nothing -> (lhs_st, bot_act)
            Just rhs_addr -> (write_memory lhs_st rhs_addr rhs_nval, write_act_addr rhs_addr)
        final_acts = acts `join_act` lhs_nact `join_act` rhs_nact
    set_state rhs_st
    trace ("interval_leq: lhs_val = " ++ show lhs_val ++ ", rhs_val = " ++ show rhs_val ++ ", lhs_nval = " ++ show lhs_nval ++ ", rhs_nval = " ++ show rhs_nval) $ return (lhs_nval, final_acts)

-- | Transformer for an expression with a single state
transformer :: SExpression -> IntTOp (IntValue,IntAct)
transformer e = trace ("transformer: " ++ show e) $
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
    Var ident -> trace ("calling var_trans" ++ show ident) $ var_transformer ident 
    ComplexReal expr -> error "transformer: complex op not supported" 
    ComplexImag expr -> error "transformer: complex op not supported" 

index_transformer :: SExpression -> SExpression -> IntTOp (IntValue, IntAct)
index_transformer lhs rhs = trace ("index_transformer: lhs = " ++ show lhs ++ ", rhs = " ++ show rhs) $ do 
  s@IntTState{..} <- get
  case get_addrs_just st scope lhs of
    MemAddrTop -> error $ "index_transformer: lhs of index operation points to MemAddrTop"
    MemAddrs l -> do
      (rhs_vals, rhs_acts) <- transformer rhs
      let l' = map (flip set_offset rhs_vals) l 
          addrs = MemAddrs l'
          vals = read_memory st addrs 
          val = join_intval_list vals
          res_acts = read_act_addr addrs `join_act` rhs_acts 
      trace ("index_transformer: res_acts = " ++ show res_acts) $ return (val, res_acts)    

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
  let lhs_id = get_addrs_just st scope lhs
      res_acts = add_writes lhs_id (rhs_acts `join_act` lhs_acts)
  -- modify the state of the addresses with
  -- the result values 
  let res_st = write_memory st lhs_id res_vals
  trace ("assign_transformer: new state \n " ++ show res_st ) $ set_state res_st 
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
        th_id = get_addrs_just s' scope (args !! 0) 
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
    let mutex_addr = get_addrs_just st scope (args !! 0)
        res_st = write_memory st mutex_addr one 
    set_state res_st
    return (one, lock_act_addr mutex_addr) 
  "pthread_mutex_unlock" -> do
    -- this transformer is only called if it is enabled 
    s@IntTState{..} <- get
    let mutex_addr = get_addrs_just st scope (args !! 0)
        res_st = write_memory st mutex_addr zero 
    set_state res_st
    return (zero, unlock_act_addr mutex_addr) 
  "pthread_mutex_init" -> do
    -- this transformer is only called if it is enabled 
    s@IntTState{..} <- get
    let mutex_addr = get_addrs_just st scope (args !! 0)
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
var_transformer sym_id = trace ("var_transformer: sym_id = " ++ show sym_id) $ do
  s@IntTState{..} <- get
  let id_addr_base = MemAddrBase sym_id scope
      id_addrs = get_full_addrs st id_addr_base 
      vals = read_memory st id_addrs 
      val = join_intval_list vals 
      acts = read_act_addr id_addrs 
  return (val, acts)
  
-- negates logical expression using De Morgan Laws
negExp :: SExpression -> SExpression
negExp expr = case expr of
  Binary CLndOp l r -> Binary CLorOp (negExp l) (negExp r)
  Binary CLorOp l r -> Binary CLndOp (negExp l) (negExp r)
  Binary op l r -> Binary (negOp op) l r
  Unary CNegOp e -> negExp e
  _ -> error $ "negExp: unsupported " ++ show expr

negOp :: BinaryOp -> BinaryOp 
negOp op = case op of
  CLeOp -> CGeqOp
  CGrOp -> CLeqOp
  CLeqOp -> CGrOp
  CGeqOp -> CLeOp
  CEqOp -> CNeqOp
  CNeqOp -> CEqOp
  _ -> error $ "negOp: unsupported " ++ show op

{-
-- apply logical operations
-- if this function returns nothing is because the condition is false
applyLogic :: Sigma -> OpCode -> Expression -> Expression -> Maybe Sigma
applyLogic s op lhs rhs =
-}

