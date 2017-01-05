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

import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)  
import qualified Debug.Trace as T
import Data.List 
import qualified Data.Set as S

import Domain.Interval.State
import Domain.Interval.Value
import Domain.Interval.Type
import Domain.Interval.API
import Domain.Action
import Domain.MemAddr
import Domain.Util

import Language.C.Syntax.Ops 
import Language.C.Syntax.Constants
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Flow
import Language.SimpleC.Util
import qualified Model.GCS as GCS
import Util.Generic hiding (safeLookup)
import qualified Debug.Trace as T

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

join_state :: IntState -> IntTOp ()
join_state nst = do
  s@IntTState{..} <- get
  let fst = nst `join_intstate` st
  put s { st = fst }

-- | converts the front end into a system
-- @REVISED: July'16
convert :: FrontEnd () (IntState,IntAct) -> GCS.System IntState IntAct
convert fe = 
  let (pos_main,sym_main) = get_entry "main" (cfgs fe) (symt fe)
      init_tstate = IntTState Global empty_state (symt fe) (cfgs fe) False
      (acts,s@IntTState{..}) = runState (transformer_decls $ decls $ ast fe) init_tstate
      st' = set_pos st (symId sym_main) sym_main pos_main  
  in trace ("convert: " ++ show (cfgs fe)) $ GCS.System st' acts (cfgs fe) (symt fe) [GCS.main_tid] 1

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
transformer_decls =
  foldM (\a d -> transformer_decl d >>= \a' -> return $ join_act a a') bot_act 

-- | transformer for a declaration:
transformer_decl :: SDeclaration -> IntTOp IntAct
transformer_decl decl = trace ("transformer_decl: " ++ show decl) $ do
  case decl of
    TypeDecl ty -> error "trannsformer_decl: not supported yet"
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
transformer_init id ty minit = trace ("transformer_init for " ++ show id ++ " with init " ++ show minit) $ do
  case minit of
    Nothing -> do
      s@IntTState{..} <- get
      let val = default_value ty
          id_addr = MemAddr id 0 scope
          st' = write_memory_addr st id_addr val  
          acts = write_act_addr $ MemAddrs [id_addr]
      set_state st'
      return acts
    Just i  -> case i of
      InitExpr expr -> do
        -- for each state, we need to apply the transformer
        s@IntTState{..} <- get
        (val, acts) <- transformer expr
        let id_addr = MemAddr id 0 scope
            st' = write_memory_addr st id_addr val 
            acts' = add_writes (MemAddrs [id_addr]) acts
        set_state st'
        return acts'
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
default_value :: Ty SymId () -> IntValue 
default_value ty = --  [ ConVal $ init_value ty ]
  InterVal (I 0, I 0)

-- | Transformers for interval semantics 
-- Given an initial state and an expression
-- return the updated state.
transformer_expr :: SExpression -> IntTOp IntAct
transformer_expr expr = trace ("transformer_expr: " ++ show expr) $ do
  s@IntTState{..} <- get
  if cond
  then bool_transformer_expr expr
  else do
    (vals,act) <- transformer expr
    return act 
    
-- eval logical expressions
-- we are going to be conservative;
-- all the variables in this expression
-- are going to be considered written
bool_transformer_expr :: SExpression -> IntTOp IntAct
bool_transformer_expr expr = trace ("bool_transformer") $ case expr of
  Binary op lhs rhs -> do 
    (val,act) <- apply_logic op lhs rhs
    return act
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
      lhs_act <- bool_transformer_expr lhs 
      rhs_act <- bool_transformer_expr rhs
      return $ (error "CLndOp",lhs_act `join_act` rhs_act)
    CLorOp -> do
      lhs_act <- bool_transformer_expr lhs
      s@IntTState{..} <- get
      if is_bot st
      then do
        rhs_act <- bool_transformer_expr rhs
        return $ (error "CLorOp",lhs_act `join_act` rhs_act)
      else return (error "CLorOp",lhs_act) 

-- Logical Operations
-- Less than (CLeOp)
-- Need to update the variables
interval_leq :: SExpression -> SExpression -> IntTOp (IntValue,IntAct)
{-
interval_leq (Ident x_i) rhs =
  let v' = lowerBound $ eval rhs s
      x = BS.pack x_i
      x_val = safeLookup "interval_leq" s x
  in case x_val of
    Bot -> Nothing
    Interval (a,b) ->
      if a <= v'
      then Just $ insert x (i a (min b v')) s
      else Nothing
    _ -> error "interval_leq"
interval_leq s lhs (Ident x_i) =
  let lhs_val = eval lhs s
      x = BS.pack x_i
      x_val = safeLookup "interval_leq" s x
      aux = i (upperBound lhs_val) PlusInf
      res = aux `iMeet` x_val
  in case res of
    Bot -> Nothing
    _ -> Just $ insert x res s
-}
interval_leq lhs rhs = do
  (lhs_val,lhs_act) <- transformer lhs 
  (rhs_val,rhs_act) <- transformer rhs 
  return (lhs_val `iMeet` rhs_val, lhs_act `join_act` rhs_act)

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
    Index arr_expr index_expr -> error "transformer: index not supported"
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
  let lhs_id = get_addrs st scope lhs
      res_acts = add_writes lhs_id (rhs_acts `join_act` lhs_acts)
  -- modify the state of the addresses with
  -- the result values 
  let res_st = write_memory st lhs_id res_vals
  set_state res_st 
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
  "pthread_create" -> do
    s@IntTState{..} <- get
        -- write in the address of (args !! 0) of type
        -- pthread_t, the pid of the new thread
    let (tid,s') = inc_num_th st
        th_id = get_addrs s' scope (args !! 0) 
        s'' = write_memory s' th_id (IntVal [VInt tid]) 
        -- retrieve the entry point of the thread code 
        th_sym = get_expr_id $ args !! 2
        th_name = get_symbol_name th_sym sym
        (th_pos,_) = get_entry th_name i_cfgs sym
        --
        i_th_st = bot_th_state th_pos th_sym
        res_st = insert_thread s'' tid i_th_st 
    set_state res_st 
    return (IntVal [], create_thread_act (SymId tid) zero) 
  "pthread_join" -> do
    s@IntTState{..} <- get
    -- this transformer is only called if it is enabled 
    let tid = get_tid_expr scope st (args !! 0)
    return (IntVal [], join_thread_act (SymId tid) zero) 
  "nondet" -> do 
    (lVal,lacts) <- transformer $ args !! 0
    (uVal,uacts) <- transformer $ args !! 1
    case (lVal,uVal) of 
      (IntVal [VInt l],IntVal [VInt u]) -> 
       return (InterVal (I l, I u),lacts `join_act` uacts)
      (l, u) -> 
       return (l `iJoin` u,lacts `join_act` uacts)
  _ -> error $ "call_transformer_name: calls to " ++ name ++ " not supported" 

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
    CPostIncOp -> error "unop_transformer: CPostIncOp not supported"   
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
var_transformer sym_id = do
  s@IntTState{..} <- get
  let id_addr = MemAddr sym_id zero scope
      val = read_memory_addr st id_addr
      acts = read_act sym_id zero scope
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

