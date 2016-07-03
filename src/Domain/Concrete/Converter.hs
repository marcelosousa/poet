{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Converter
-- Copyright :  (c) 2015-16 Marcelo Sousa
--
-- Transformers for the concrete semantics.
-- Two main transformers for the types of edges in the CFG:
--   transformer_decl (for the declaration)
--   transformer_expr (for the expression) 
-------------------------------------------------------------------------------
module Domain.Concrete.Converter where

import Control.Monad.State.Lazy 

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Map (Map)  
import Debug.Trace
import Data.List 
import qualified Data.Set as S

import Domain.Concrete.Action
import Domain.Concrete.State

import Language.C.Syntax.Ops 
import Language.SimpleC.AST hiding (Value)
import Language.SimpleC.Converter hiding (Scope(..))
import Language.SimpleC.Flow
import Language.SimpleC.Util
import Model.GCS
import Util.Generic hiding (safeLookup)

-- | State of the concrete transformer
data ConTState 
 = ConTState {
   scope :: Scope
 , st :: CState            -- the set of states
 , cst :: Sigma            -- current state in the state 
 , sym :: Map SymId Symbol
 , cfgs :: Graphs SymId () (CState,Act)
 , cond :: Bool            -- is a condition? 
 }

-- | Transformer operation 
type ConTOp val = State ConTState val

set_state :: CState -> ConTOp ()
set_state nst = do
  s@ConTState{..} <- get
  put s { st = nst }

join_state :: CState -> ConTOp ()
join_state nst = do
  s@ConTState{..} <- get
  let fst = nst `join_cstate` st
  put s { st = fst }

-- | Picks a state from the set and sets
--   as the current one
set_single_state :: Sigma -> ConTOp () 
set_single_state state = do
  s@ConTState{..} <- get
  put s { cst = state }

-- | converts the front end into a system
-- @REVISED: July'16
convert :: FrontEnd () (CState,Act) -> System CState Act
convert fe@FrontEnd{..} =
  let pos_main = get_entry "main" cfgs symt
      init_tstate = ConTState Global empty_state bot_sigma symt cfgs False
      (acts,s@ConTState{..}) = runState (transformer_decls $ decls ast) init_tstate
      st' = set_pos st main_tid pos_main  
  in System st' acts cfgs symt [main_tid] 1

-- | retrieves the entry node of the cfg of a function
get_entry :: String -> Graphs SymId () (CState,Act) -> Map SymId Symbol -> Pos
get_entry fnname graphs symt = 
  case M.foldrWithKey aux_get_entry Nothing graphs of
    Nothing -> error $ "get_entry: cant get entry for " ++ fnname
    Just p -> p
 where 
   aux_get_entry sym cfg r =
     case r of 
       Just res -> Just res
       Nothing -> 
         case M.lookup sym symt of
           Nothing -> Nothing
           Just s  -> if get_name s == fnname
                      then Just $ entry_node cfg
                      else Nothing 

-- | processes a list of declarations 
transformer_decls :: [SDeclaration] -> ConTOp Act
transformer_decls =
  foldM (\a d -> transformer_decl d >>= \a' -> return $ join_act a a') bot_act 

-- | transformer for a declaration:
transformer_decl :: SDeclaration -> ConTOp Act
transformer_decl decl = do
  s@ConTState{..} <- get
  let states = S.toList $ sts st
  -- reset the states
  set_state $ CState S.empty
  -- for each previous state; run the transformer on that state
  acts <- mapM (\st -> set_single_state st >> transformer_decl_) states
  -- join all the actions 
  return $ foldr join_act bot_act acts
 where
   --   this transformer already has a particular state
   --   to process within the set.
   transformer_decl_ :: ConTOp Act
   transformer_decl_ = 
     case decl of
       TypeDecl ty -> error "convert_decl: not supported yet"
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
transformer_init :: SymId -> STy -> Maybe (Initializer SymId ()) -> ConTOp Act
transformer_init id ty minit = do
  case minit of
    Nothing -> do
      s@ConTState{..} <- get
      let val = default_value ty
          st' = case scope of 
                  Global -> insert_heap cst id ty val
                  Local i -> insert_local cst i id val 
          id_addrs = get_addrs_id cst scope id
          acts = Act bot_maddrs id_addrs bot_maddrs bot_maddrs
      join_state st'
      return acts
    Just i  ->
      case i of
        InitExpr expr -> do
          -- for each state, we need to apply the transformer
          s@ConTState{..} <- get
          (val,acts) <- transformer expr
          let st' = case scope of
                      Global -> insert_heap cst id ty val
                      Local i -> insert_local cst i id val 
              id_addrs = get_addrs_id cst scope id
              -- this is wrong because id_addrs can be shared among threads
              acts' = add_writes id_addrs acts
          join_state st'
          return acts'
        InitList list -> error "initializer list is not supported"

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
default_value :: Ty SymId () -> ConValues
default_value ty = [ ConVal $ init_value ty ]

-- | Transformers for concrete semantics 
-- Given an initial state and an expression
-- return the updated state, the set of values
-- of this expression and a set of actions
-- performed by this expression.
transformer_expr :: SExpression -> ConTOp Act
transformer_expr expr = do
  s@ConTState{..} <- get
  let states = S.toList $ sts st
  -- reset the states
  set_state $ CState S.empty
  -- for each previous state; run the transformer on that state
  valsacts <- mapM (\st -> set_single_state st >> transformer expr >>= \(a,b) -> return (st,a,b)) states
  if cond
  then do
    -- filter the states and compute the actions
    let (nsts,nvals) = foldr cond_transformer_expr ([],bot_act) valsacts
    -- add the states
    join_state $ CState $ S.fromList nsts 
    return nvals 
  -- join all the actions
  else return $ foldr (\(_,_,a) r -> a `join_act` r) bot_act valsacts
 where
   cond_transformer_expr (st,vals,acts) (sts,facts) =
     if any isTrue vals
     then (st:sts,acts `join_act` facts)
     else (sts,acts `join_act` facts)
     
-- | Transformer for an expression with a single state
transformer :: SExpression -> ConTOp (ConValues,Act)
transformer e =
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
    Skip -> return ([],bot_act)
    StatExpr stmt -> error "transformer: stat_expr not supported"
    Unary unaryOp expr -> unop_transformer unaryOp expr 
    Var ident -> var_transformer ident 
    ComplexReal expr -> error "transformer: complex op not supported" 
    ComplexImag expr -> error "transformer: complex op not supported" 

-- | Transformer for an assignment expression.
--   For each state in the domain, 
-- given one state, we are going to update it 
-- by assigning to the lhs the set of possible values.
-- Note that this can expand the set of states.
assign_transformer :: AssignOp -> SExpression -> SExpression -> ConTOp (ConValues,Act)
assign_transformer op lhs rhs = do
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_vals = case op of
        CAssignOp -> rhs_vals
        -- arithmetic operations 
        CMulAssOp -> lhs_vals `mult` rhs_vals 
        CDivAssOp -> lhs_vals `divs` rhs_vals 
        CRmdAssOp -> lhs_vals `rmdr` rhs_vals 
        CAddAssOp -> lhs_vals `add` rhs_vals 
        CSubAssOp -> lhs_vals `sub` rhs_vals
        -- bit-wise operations 
        CShlAssOp -> lhs_vals `shl` rhs_vals 
        CShrAssOp -> lhs_vals `shr` rhs_vals 
        CAndAssOp -> lhs_vals `band` rhs_vals 
        CXorAssOp -> lhs_vals `xor` rhs_vals 
        COrAssOp  -> lhs_vals `bor` rhs_vals
  -- get the addresses of the left hand side
  s@ConTState{..} <- get
  let lhs_id = get_addrs cst scope lhs
      res_acts = add_writes lhs_id (rhs_acts `join_act` lhs_acts)
  -- modify the state of the addresses with
  -- the result values 
  let res_st = modify_state scope cst lhs_id res_vals
  join_state res_st 
  return (res_vals,res_acts) 

-- | Transformer for binary operations.
--   These transformers do not change the state;
binop_transformer :: BinaryOp -> SExpression -> SExpression -> ConTOp (ConValues,Act)
binop_transformer binOp lhs rhs = do
  s@ConTState{..} <- get
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_vals = case binOp of
        CMulOp -> lhs_vals `mult` rhs_vals 
        CDivOp -> lhs_vals `divs` rhs_vals 
        CRmdOp -> lhs_vals `rmdr` rhs_vals 
        CAddOp -> lhs_vals `add` rhs_vals 
        CSubOp -> lhs_vals `sub` rhs_vals
        -- boolean operations 
        CLeOp  -> lhs_vals `le` rhs_vals 
        CGrOp  -> lhs_vals `gr` rhs_vals 
        CLeqOp -> lhs_vals `leq` rhs_vals  
        CGeqOp -> lhs_vals `geq` rhs_vals 
        CEqOp  -> lhs_vals `eq` rhs_vals 
        CNeqOp -> lhs_vals `neq` rhs_vals 
        CAndOp -> lhs_vals `land` rhs_vals 
        COrOp  -> lhs_vals `lor` rhs_vals
        -- bit-wise operations 
        CShlOp -> error "binop_transformer: CShlOp not supported"  
        CShrOp -> error "binop_transformer: CShrOp not supported" 
        CXorOp -> error "binop_transformer: CXorOp not supported" 
        CLndOp -> error "binop_transformer: CLndOp not supported" 
        CLorOp -> error "binop_transformer: CLorOp not supported"
      res_acts = lhs_acts `join_act` rhs_acts
  return (res_vals,res_acts)

-- | Transformer for call expression:
--   Support for pthread API functions
--   pthread_create
--   pthread_join
--   lock
--   unlock  
call_transformer :: SExpression -> [SExpression] -> ConTOp (ConValues,Act)
call_transformer fn args =
  case fn of
    Var ident -> do
      s@ConTState{..} <- get
      let n = get_symbol_name ident sym
      call_transformer_name n args 
    _ -> error "call_transformer: not supported" 

call_transformer_name :: String -> [SExpression] -> ConTOp (ConValues,Act)
call_transformer_name name args = case name of
  "pthread_create" -> do
    s@ConTState{..} <- get
    let th_id = get_expr_id $ args !! 1
        th_sym = get_expr_id $ args !! 3
        th_name = get_symbol_name th_sym sym
        th_pos = get_entry th_name cfgs sym
        st' = insert_thread cst th_id th_pos
    join_state st' 
    return ([],bot_act) 
  _ -> error $ "call_transformer_name: calls to " ++ name ++ " not supported" 

cond_transformer :: SExpression -> Maybe SExpression -> SExpression -> ConTOp (ConValues,Act)
cond_transformer cond mThen else_e = do
  (vals,acts) <- transformer cond
  -- vals is both false and true 
  let (isTrue,isFalse) = checkBoolVals vals
  (tVal,tAct) <-
    if isTrue
    then case mThen of
           Nothing -> error "cond_transformer: cond is true and there is not then"
           Just e -> transformer e
    else return ([],bot_act)
  (eVal,eAct) <-
    if isFalse
    then transformer else_e
    else return ([],bot_act)
  let res_vals = tVal ++ eVal
      res_acts = acts `join_act` tAct `join_act` eAct
  return (res_vals,res_acts) 

-- | Transformer for constants.
const_transformer :: Constant -> ConTOp (ConValues,Act)
const_transformer const =
  let v = toValue const
  in return ([ConVal v], bot_act)

-- | Transformer for unary operations.
unop_transformer :: UnaryOp -> SExpression -> ConTOp (ConValues,Act)
unop_transformer unOp expr = do 
  -- process the lhs (values and actions)
  (expr_vals,res_acts) <- transformer expr 
  let res_vals = case unOp of
        CPreIncOp  -> error "unop_transformer: CPreIncOp  not supported"     
        CPreDecOp  -> error "unop_transformer: CPreDecOp  not supported"  
        CPostIncOp -> error "unop_transformer: CPostIncOp not supported"   
        CPostDecOp -> error "unop_transformer: CPostDecOp not supported"   
        CAdrOp     -> error "unop_transformer: CAdrOp     not supported"    
        CIndOp     -> error "unop_transformer: CIndOp     not supported" 
        CPlusOp    -> expr_vals 
        CMinOp     -> minus expr_vals 
        CCompOp    -> error "unop_transformer: CCompOp not supported" 
        CNegOp     -> neg_tr expr_vals 
  return (res_vals,res_acts)

-- | Transformer for var expressions.
var_transformer :: SymId -> ConTOp (ConValues,Act)
var_transformer id = do
  s@ConTState{..} <- get
  -- First search in the heap
  case M.lookup id (heap cst) of
    Nothing -> 
      -- If not in the heap, search in the thread
      case scope of
        Global -> error "var_transformer: id is not the heap and scope is global"
        Local i -> case M.lookup i (th_states cst) of
          Nothing -> error "var_transformer: scope does not match the state"
          Just ths@ThState{..} -> case M.lookup id locals of
            Nothing -> error "var_transformer: id is not in the local state of thread"
            Just v  -> do
              let reds = Act (MemAddrs [MemAddr id scope]) bot_maddrs bot_maddrs bot_maddrs 
              return ([v],reds)
    Just cell@MCell{..} -> do
      let reds = Act (MemAddrs [MemAddr id Global]) bot_maddrs bot_maddrs bot_maddrs 
      return ([val],reds)
  
-- | get the addresses of an identifier
--   super simple now by assuming not having pointers
get_addrs_id :: Sigma -> Scope -> SymId -> MemAddrs
get_addrs_id cst scope id = 
  case M.lookup id (heap cst) of
    Nothing -> MemAddrs [MemAddr id scope] 
    Just i  -> MemAddrs [MemAddr id Global] 

-- | get_addrs retrieves the information from the 
--   points to analysis.
--   Simplify to onlu consider the case where the 
--   the expression is a LHS (var or array index).
get_addrs :: Sigma -> Scope -> SExpression -> MemAddrs
get_addrs st scope expr =
  case expr of
    Var id -> get_addrs_id st scope id 
    _ -> error "get_addrs: not supported"

mult = undefined
divs = undefined
rmdr = undefined
add = undefined
sub = undefined
shl = undefined
shr = undefined
band = undefined
bor = undefined
xor = undefined
le = undefined
gr  = undefined
leq = undefined
geq = undefined
eq  = undefined
neq = undefined
land = undefined
lor = undefined

-- Unary operations
minus, neg_tr :: ConValues -> ConValues
minus = undefined
neg_tr = undefined
