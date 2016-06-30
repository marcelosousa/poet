{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Domain.Concrete.Converter
-- Copyright :  (c) 2015-16 Marcelo Sousa
--
-- Transformers for the concrete semantics.
-- 
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
 , st :: CState 
 , sym :: Map SymId Symbol 
 }

-- | Transformer operation 
type ConTOp val = State ConTState val

set_state :: CState -> ConTOp ()
set_state nst = do
  s@ConTState{..} <- get
  put s { st = nst }
 
-- | The interface
instance Collapsible CState Act where
  enabled = undefined
  collapse = undefined
  dcollapse = undefined 

-- | converts the front end into a system
convert :: FrontEnd () (CState,Act) -> System CState Act
convert fe@FrontEnd{..} =
  let pos_main = get_entry "main" cfgs symt
      init_tstate = ConTState Global empty_state symt
      (acts,s@ConTState{..}) = runState (convert_decls $ decls ast) init_tstate
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
convert_decls :: [SDeclaration] -> ConTOp Act
convert_decls =
  foldM (\a d -> convert_decl d >>= \a' -> return $ join_act a a') bot_act 

-- | processes a declaration.
convert_decl :: SDeclaration -> ConTOp Act
convert_decl decl = 
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
              in  convert_init id typ initializer

-- | processes a declaration initializer 
--   for the symbol id with type ty. 
convert_init :: SymId -> STy -> Maybe (Initializer SymId ()) -> ConTOp Act
convert_init id ty minit = do
  case minit of
    Nothing -> do
      s@ConTState{..} <- get
      let val = default_value ty
          st' = case scope of 
                  Global -> insert_heap st id ty val
                  Local i -> insert_local st i id val 
          id_addrs = get_addrs_id scope st id
          acts = Act bot_maddrs id_addrs bot_maddrs bot_maddrs
      set_state st'
      return acts
    Just i  ->
      case i of
        InitExpr expr -> do 
          (val,acts) <- transformer expr
          s@ConTState{..} <- get
          let st' = case scope of
                      Global -> insert_heap st id ty val
                      Local i -> insert_local st i id val 
              id_addrs = get_addrs_id scope st id
              acts' = add_writes id_addrs acts
          set_state st'
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
transformer :: SExpression -> ConTOp (ConValues,Act)
transformer _expr =
  case _expr of 
    AlignofExpr expr -> error "transformer: align_of_expr not supported"  
    AlignofType decl -> error "transformer: align_of_type not supported"
    Assign assignOp lhs rhs -> assign_transformer assignOp lhs rhs 
    Binary binaryOp lhs rhs -> binop_transformer binaryOp lhs rhs 
    BuiltinExpr built -> error "transformer: built_in_expr not supported" 
    Call fn args n -> call_transformer fn args  
    Cast decl expr -> error "transformer: cast not supported"
    Comma exprs -> error "transformer: comma not supported" 
    CompoundLit decl initList -> compound_transformer decl initList 
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
--   Not sure if we should have a specialized transformer for this 
--   case or if the expansion to the full expression is enough.
assign_transformer :: AssignOp -> SExpression -> SExpression -> ConTOp (ConValues,Act)
assign_transformer op lhs rhs = do
  s@ConTState{..} <- get
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
      lhs_id = get_addrs scope st lhs
      res_acts = add_writes lhs_id (rhs_acts `join_act` lhs_acts)
  s@ConTState{..} <- get 
  -- modify the state of the addresses with
  -- the result values 
  let res_st = modify_state scope st lhs_id res_vals
  set_state res_st 
  return (res_vals,res_acts) 

-- | Transformer for binary operations.
binop_transformer :: BinaryOp -> SExpression -> SExpression -> ConTOp (ConValues,Act)
binop_transformer binOp lhs rhs = do
  -- process the lhs (get the new state, values and actions)
  (lhs_vals,lhs_acts) <- transformer lhs
  -- process the rhs (get the new state, values and actions)
  (rhs_vals,rhs_acts) <- transformer rhs
  let res_vals = case binOp of
        CMulOp -> error "binop_transformer: not supported" 
        CDivOp -> error "binop_transformer: not supported" 
        CRmdOp -> error "binop_transformer: not supported" 
        CAddOp -> error "binop_transformer: not supported" 
        CSubOp -> error "binop_transformer: not supported" 
        CLeOp  -> error "binop_transformer: not supported" 
        CGrOp  -> error "binop_transformer: not supported" 
        CLeqOp -> error "binop_transformer: not supported"  
        CGeqOp -> error "binop_transformer: not supported" 
        CEqOp  -> error "binop_transformer: not supported" 
        CNeqOp -> error "binop_transformer: not supported" 
        CAndOp -> error "binop_transformer: not supported" 
        COrOp  -> error "binop_transformer: not supported" 
        CShlOp -> error "binop_transformer: not supported"  
        CShrOp -> error "binop_transformer: not supported" 
        CXorOp -> error "binop_transformer: not supported" 
        CLndOp -> error "binop_transformer: not supported" 
        CLorOp -> error "binop_transformer: not supported"
      res_acts = lhs_acts `join_act` rhs_acts
  return (res_vals,res_acts)

-- | Transformer for call expression.
call_transformer :: SExpression -> [SExpression] -> ConTOp (ConValues,Act)
call_transformer fn args = undefined

-- | Transformer for a declaration expression.
compound_transformer :: SDeclaration -> InitializerList SymId () -> ConTOp (ConValues,Act)
compound_transformer = undefined

cond_transformer :: SExpression -> Maybe SExpression -> SExpression -> ConTOp (ConValues,Act)
cond_transformer = undefined
 
-- | Transformer for constants.
const_transformer :: Constant -> ConTOp (ConValues,Act)
const_transformer const =
  let v = toValue const
  in return ([ConVal v], bot_act)

-- | Transformer for unary operations.
unop_transformer :: UnaryOp -> SExpression -> ConTOp (ConValues,Act)
unop_transformer = undefined

-- | Transformer for var expressions.
var_transformer :: SymId -> ConTOp (ConValues,Act)
var_transformer id = do
  s@ConTState{..} <- get
  mapM var_transformer_
  -- First search in the heap
  case M.lookup id (heap st) of
    Nothing -> 
      -- If not in the heap, search in the thread
      case scope of
        Global -> error "var_transformer: id is not the heap and scope is global"
        Local i -> case M.lookup i (th_states st) of
          Nothing -> error "var_transformer: scope does not match the state"
          Just ths@ThState{..} -> case M.lookup id locals of
            Nothing -> error "var_transformer: id is not in the local state of thread"
            Just v  -> return (v,bot_act)
    Just cell@MCell{..} -> return (val,bot_act)
  
-- | get the addresses of an identifier
--   super simple now by assuming not having pointers
get_addrs_id :: Scope -> CState -> SymId -> MemAddrs
get_addrs_id scope st id = MemAddrs [MemAddr id] 

-- | get_addrs retrieves the information from the 
--   points to analysis.
--   Simplify to onlu consider the case where the 
--   the expression is a LHS (var or array index).
get_addrs :: Scope -> CState -> SExpression -> MemAddrs
get_addrs scope st expr =
  case expr of
    Var id -> get_addrs_id scope st id 
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
{-
pmdVar = BS.pack "__poet_mutex_death"
pmdVal = IntVal 1
pmtVar = BS.pack "__poet_mutex_threads"
pmjVar = BS.pack "__poet_mutex_threads_join"

convert :: Program -> FirstFlow -> Flow -> Int -> (System CState, UIndep)
convert (Program (decls, defs)) pcs flow thCount =
  -- @ get the initial local state: this will be the set of global variables 
  --   minus the pcs
  let ils = getInitialDecls decls
      pmtiv = Array $ map IntVal $ replicate thCount 1
      ipcs = map (\(i,pc) -> (BS.pack ("pc."++i), IntVal pc)) pcs
      iils = ils++ipcs
      fils = (pmtVar, pmtiv):(pmjVar, pmtiv):iils
      is = toCState fils
      atrs = resetTID $ concatMap (getTransitions flow) defs
      (trs,annot) = unzip atrs
--      vtrs = trace ("transitions = " ++ concatMap showTransition trs ++ "\n" ++ show annot) $ V.fromList trs
      vtrs = V.fromList trs
      uind = computeUIndep annot
      sys = System vtrs is $ (Lock (V pmdVar)):[Lock (A pmtVar (toInteger th)) | th <- [0 .. thCount-1]] ++ [Lock (A pmjVar (toInteger th)) | th <- [0 .. thCount-1]]
  in (sys, uind)       
  --trace ("fromConvert: transitions = " ++ concatMap showTransition trs) $ return (sys, uind) 

resetTID :: [(Transition CState, (TransitionID, Statement, RWSet))] -> [(Transition CState, (TransitionID, Statement, RWSet))] 
resetTID = reverse . snd . foldl (\(cnt,rest) l -> let (ncnt,l') = resetTID' cnt l in (ncnt,l':rest)) (0,[])

resetTID' :: Int -> (Transition CState, (TransitionID, Statement, RWSet)) -> (Int, (Transition CState, (TransitionID, Statement, RWSet)))
resetTID' c (((pid,_,_st,act),fn),(_,st,annot)) = (c+1,(((pid,c,_st,act),fn),(c,st,annot)))

getInitialDecls :: Decls -> LCState
getInitialDecls = foldl (\a decl -> convertDecl decl ++ a) [] 
  where 
    convertDecl decl = case decl of
      FunctionDecl _ _ _ -> [] 
      GlobalDecl _ (Ident i) Nothing -> [(BS.pack i, IntVal 0)]
      GlobalDecl _ (Ident i) (Just (Const (IntValue v))) -> [(BS.pack i, (IntVal $ fromInteger v))]
      GlobalDecl _ (Index (Ident i) _) _ -> [] --error "getInitialDecls: global array is not supported yet"
      _ -> error "getInitialState: not supported yet"

-- for each transition: 
-- type Transition s = (ProcessID, TransitionID, TransitionFn s)
-- process id is the name of the function
-- transition id is the position in the vector of transitions 
getTransitions :: Flow -> Definition -> [(Transition CState, (TransitionID, Statement, RWSet))] 
getTransitions flow (FunctionDef _ name _ stat) = recGetTrans flow (BS.pack name) stat

recGetTrans :: Flow -> ProcessID -> Statement -> [(Transition CState, (TransitionID, Statement, RWSet))] 
recGetTrans flow name stat =
    foldl (\acc st -> let rest = toTransition name 0 flow st
                      in acc++rest) [] stat    

toTransition :: ProcessID -> TransitionID -> Flow -> AnnStatement PC -> [(Transition CState, (TransitionID, Statement, RWSet))]
toTransition procName tID flow s = 
  let pcVar = BS.pack $ "pc." ++ (BS.unpack procName)
      trInfo = \act -> (procName, tID, [s], act)
      trInfoDefault = trInfo [Other]
  in case s of
      ExprStat pc _expr ->
        case _expr of
          Call fname args ->
            let trrws = fromCall flow pcVar pc fname args
            in map (\(tr, act, rw) -> ((trInfo act, tr), (tID,[s],rw))) trrws
          Assign _ _lhs _rhs ->
            let trrws = fromAssign flow pcVar pc _lhs _rhs
            in map (\(tr, rw) -> ((trInfoDefault, tr), (tID,[s],rw))) trrws
      If pc _cond _then _else ->
        let trrws = fromIf flow pcVar pc _cond 
            _thentr = recGetTrans flow procName _then
            _elsetr = recGetTrans flow procName _else
            _condtr = map (\(tr, rw) -> ((trInfoDefault, tr), (tID,[s],rw))) trrws 
        in _condtr ++ _thentr ++ _elsetr
      IfThen pc _cond _then ->
        let trrws = fromIf flow pcVar pc _cond 
            _thentr = recGetTrans flow procName _then
            _condtr = map (\(tr, rw) -> ((trInfoDefault, tr), (tID,[s],rw))) trrws 
        in _condtr ++ _thentr
      Goto pc loc -> 
        let trrws = fromGoto flow pcVar pc
        in map (\(tr, rw) -> ((trInfoDefault, tr), (tID,[s],rw))) trrws 
      _ -> error $ "toTransition: " ++ show s
        
modifyList :: [a] -> a -> Integer -> [a]
modifyList xs a idx = 
  let (left,_:right) = splitAt (fromInteger idx) xs
  in left ++ (a:right)
      
-- encodes Call
fromCall :: Flow -> Var -> PC -> String -> [Expression] -> [(TransitionFn CState, Acts, RWSet)]
fromCall flow pcVar pc "__poet_fail" [] =
  let acts = [Write (V pcVar)]
      fn = \s ->
        let IntVal curPC = safeLookup "call" s pcVar
        in if curPC == pc
           then error "poet found an assertion violation!"
           else []
  in [(fn, [Other], acts)]
fromCall flow pcVar pc name [param] = 
  let Continue next = getFlow flow pc
  in case name of 
    "__poet_mutex_lock" ->
      case param of
        -- @ Lock Variable
        Ident i -> 
          let ident = BS.pack i
              acts = [Write (V pcVar), Write (V ident)]
              act = [Lock $ V ident]
              fn = \s ->
                let IntVal curPC = safeLookup "call" s pcVar
                    IntVal v = safeLookup "call" s ident
                in if curPC == pc && v == 0
                   then 
                     let pcVal = IntVal next
                         iVal = IntVal 1
                     in [insert pcVar pcVal $ insert ident iVal s]
                   else []
          in [(fn, act, acts)]
        -- @ Array of Locks              
        Index (Ident i) (Const (IntValue idx)) ->
          let ident = BS.pack i
              acts = [Write (V pcVar), Write (A ident idx)]
              act = [Lock $ A ident idx]              
              fn = \s ->
                let IntVal curPC = safeLookup "call" s pcVar
                    Array vs = safeLookup "call" s ident
                    IntVal v = vs!!(fromInteger idx)
                in if curPC == pc && v == 0
                   then
                     let pcVal = IntVal next
                         vs' = modifyList vs (IntVal 1) idx
                         iVal = Array vs'
                     in [insert pcVar pcVal $ insert ident iVal s]
                else []
          in [(fn, act, acts)]         
        Index (Ident i) (Ident idxident) ->
          let ident = BS.pack i
              idxi = BS.pack idxident
              acts = [Write (V pcVar), Write (V ident), Read (V idxi)]
              act = [Lock $ V ident]              
              fn = \s -> 
                let IntVal curPC = safeLookup "call lock array pc" s pcVar
                    Array vs = safeLookup ("call lock array: " ++ show ident) s ident
                    IntVal idx = safeLookup "call lock array ident" s idxi
                    IntVal v = vs!!idx
                in if curPC == pc && v == 0
                   then
                     let pcVal = IntVal next
                         vs' = modifyList vs (IntVal 1) (toInteger idx)
                         iVal = Array vs'
                     in [insert pcVar pcVal $ insert ident iVal s]
                   else []
          in [(fn, act, acts)]
    "__poet_mutex_unlock" ->
      case param of
        -- @ Lock Variable
        Ident i ->
          let ident = BS.pack i
              acts = [Write (V pcVar), Write (V ident)]
              act = [Unlock $ V ident]
              fn = \s ->
                let IntVal curPC = safeLookup "call" s pcVar
                in if curPC == pc
                   then
                     let pcVal = IntVal next
                         iVal = IntVal 0
                     in [insert pcVar pcVal $ insert ident iVal s]
                   else []
          in [(fn, act, acts)]
        -- @ Array of Locks
        Index (Ident i) (Const (IntValue idx)) ->
          let ident = BS.pack i
              acts = [Write (V pcVar), Write (A ident idx)]
              act = [Unlock $ A ident idx]
              fn = \s ->
                let IntVal curPC = safeLookup "call" s pcVar
                in if curPC == pc
                   then
                     let IntVal curPC = safeLookup "call" s pcVar
                         Array vs = safeLookup "call" s ident
                         pcVal = IntVal next
                         vs' = modifyList vs (IntVal 0) idx
                         iVal = Array vs'
                     in [insert pcVar pcVal $ insert ident iVal s]
                   else []
          in [(fn, act, acts)]     
        Index (Ident i) (Ident idxident) ->
          let ident = BS.pack i
              idxi = BS.pack idxident
              acts = [Write (V pcVar), Write (V ident), Read (V idxi)]
              act = [Unlock $ V ident]
              fn = \s ->
                let IntVal curPC = safeLookup "call" s pcVar
                in if curPC == pc
                    then
                      let IntVal curPC = safeLookup "call unlock array" s pcVar
                          Array vs = safeLookup "call unlock array " s ident
                          IntVal idx = safeLookup "call unlock array ident" s idxi
                          pcVal = IntVal next
                          vs' = modifyList vs (IntVal 0) (toInteger idx)
                          iVal = Array vs'
                      in [insert pcVar pcVal $ insert ident iVal s]
                    else []
          in [(fn, act, acts)]                 
    _ -> error "fromCall: call not supported"

getVarArg :: Expression -> Var
getVarArg (Ident i) = BS.pack i
getVarArg (Index (Ident i) _) = BS.pack i
getVarArg e = error $ "getVarArg: " ++ show e

-- encodes Assign
fromAssign :: Flow -> Var -> PC -> Expression -> Expression -> [(TransitionFn CState, RWSet)]
fromAssign flow pcVar pc _lhs _rhs = 
  let Continue next = getFlow flow pc
      _lhsi = map Write $ getIdent _lhs
      _rhsi = map Read $ getIdent _rhs
      act = (Write $ V pcVar):(_lhsi ++ _rhsi)
      fn = \s ->
        let IntVal curPC = safeLookup "goto" s pcVar
        in if curPC == pc
           then
             let pcVal = IntVal next
                 ns = insert pcVar pcVal s
                 val = eval _rhs ns
             in case _lhs of 
               Ident i ->
                 let ident = BS.pack i
                     iVal = val
                 in [insert ident iVal ns]
               Index (Ident i) (Const (IntValue idx)) ->
                 let ident = BS.pack i
                     Array vs = safeLookup "call" s ident
                     vs' = modifyList vs val idx
                     iVal = Array vs'
                 in [insert ident iVal s]
           else []
  in [(fn, act)]

-- encodes GOTO
fromGoto :: Flow -> Var -> PC -> [(TransitionFn CState, RWSet)]
fromGoto flow pcVar pc = 
  let Continue next = getFlow flow pc
      fn = \s ->
        let IntVal curPC = safeLookup "goto" s pcVar
        in if curPC == pc
           then
             let pcVal = IntVal next
             in [insert pcVar pcVal s]
           else []
  in [(fn, [Write $ V pcVar])]

-- encodes fromIf
fromIf :: Flow -> Var -> PC -> Expression -> [(TransitionFn CState, RWSet)]
fromIf flow pcVar pc _cond = 
  let Branch (t,e) = getFlow flow pc
      readVars = getIdent _cond
      annots = (Write $ V pcVar):(map Read readVars)
      fn = \s ->
        let IntVal curPC = safeLookup "if" s pcVar
            valCond = evalCond _cond s
        in if curPC == pc
           then if valCond
                then
                  let pcVal = IntVal t
                  in [insert pcVar pcVal s]
                else
                  let pcVal = IntVal e
                  in [insert pcVar pcVal s]
           else []
  in [(fn, annots)]

getIdent :: Expression -> [Variable]
getIdent expr = case expr of
  BinOp op lhs rhs -> getIdent lhs ++ getIdent rhs
  UnaryOp op rhs -> getIdent rhs
  Const v -> []
  Ident i -> [V $ BS.pack i]
  Index (Ident i) (Const (IntValue idx)) -> [A (BS.pack i) idx]
  Index (Ident i) rhs -> (V $ BS.pack i):getIdent rhs
  Call _ args -> concatMap getIdent args
  _ -> error $ "eval: disallowed " ++ show expr

eval :: Expression -> CState -> Value
eval expr s = case expr of
  BinOp op lhs rhs ->
    let lhsv = eval lhs s
        rhsv = eval rhs s
    in apply op lhsv rhsv
  UnaryOp op rhs ->
    let v = eval rhs s
    in case op of
        CPlusOp -> v
        CMinOp  -> case v of
          IntVal iv -> IntVal (-iv)
          _ -> error $ "eval: unaryop " ++ show expr
        CNegOp  -> case v of
          IntVal 0 -> IntVal 1
          IntVal 1 -> IntVal 0
          _ -> error $ "eval: unaryop " ++ show expr
        _ -> error $ "eval: disallowed unary op: " ++ show expr    
  Const (IntValue v) -> IntVal $ fromInteger v
  Ident i -> 
    let ident = BS.pack i
    in safeLookup "eval" s ident
  Index (Ident i) rhs ->
    let ident = BS.pack i
        v = safeLookup "eval" s ident  
        vhs = eval rhs s
    in case v of
      IntVal idx -> error $ "eval: fatal error " ++ show expr
      Array vs -> case vhs of
        IntVal idx -> vs!!idx
        Array _ -> error $ "eval: disallowed " ++ show expr           
  _ -> error $ "eval: disallowed " ++ show expr

apply :: OpCode -> Value -> Value -> Value
apply op (IntVal lhs) (IntVal rhs) = case op of
  CMulOp -> IntVal $ lhs * rhs
  CDivOp -> IntVal $ lhs `div` rhs
  CRmdOp -> IntVal $ lhs `mod` rhs
  CAddOp -> IntVal $ lhs + rhs
  CSubOp -> IntVal $ lhs - rhs
  CLeOp  -> IntVal $ fromBool $ lhs < rhs
  CGrOp  -> IntVal $ fromBool $ lhs > rhs
  CLeqOp -> IntVal $ fromBool $ lhs <= rhs
  CGeqOp -> IntVal $ fromBool $ lhs >= rhs
  CEqOp  -> IntVal $ fromBool $ lhs == rhs
  CNeqOp -> IntVal $ fromBool $ lhs /= rhs
  CLndOp -> IntVal $ fromBool $ toBool lhs && toBool rhs
  CLorOp -> IntVal $ fromBool $ toBool lhs || toBool rhs
apply _ _ _ = error "apply: not all sides are just integer values"

evalCond :: Expression -> CState -> Bool
evalCond expr s =
  case eval expr s of
    IntVal 0 -> False
    IntVal 1 -> True
    _ -> error $ "evalCond: " ++ show expr
-}
