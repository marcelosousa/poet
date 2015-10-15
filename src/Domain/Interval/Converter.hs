-------------------------------------------------------------------------------
-- Module    :  Domain.Interval.Converter
-- Copyright :  (c) 2015 Marcelo Sousa
-- Defines the abstract transformers
-------------------------------------------------------------------------------
module Domain.Interval.Converter where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector as V
import Debug.Trace

import Domain.Concrete.Independence
import Domain.Interval.Type

import Frontend
import Frontend.Util
import Language.SimpleC.AST hiding (Value)
import Language.C.Syntax.AST (CBinaryOp(..),CUnaryOp(..))
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

pmdVar = BS.pack "__poet_mutex_death"
pmdVal = IntVal 1
pmtVar = BS.pack "__poet_mutex_threads"
pmjVar = BS.pack "__poet_mutex_threads_join"

convert :: Program -> FirstFlow -> Flow -> Int -> (System Sigma, UIndep)
convert (Program (decls, defs)) pcs flow thCount =
  -- @ get the initial local state: this will be the set of global variables 
  --   minus the pcs
  let ils = getInitialDecls decls
      pmtiv = Array $ map IntVal $ replicate thCount 1
      ipcs = map (\(i,pc) -> (BS.pack ("pc."++i), IntVal pc)) pcs
      iils = ils++ipcs
      fils = (pmtVar, pmtiv):(pmjVar, pmtiv):iils
      is = toSigma fils
      atrs = resetTID $ concatMap (getTransitions flow) defs
      (trs,annot) = unzip atrs
--      vtrs = trace ("transitions = " ++ concatMap showTransition trs ++ "\n" ++ show annot) $ V.fromList trs
      vtrs = V.fromList trs
      uind = computeUIndep annot
      sys = System vtrs is $ (Lock (V pmdVar)):[Lock (A pmtVar (toInteger th)) | th <- [0 .. thCount-1]] ++ [Lock (A pmjVar (toInteger th)) | th <- [0 .. thCount-1]]
  in (sys, uind)       
  --trace ("fromConvert: transitions = " ++ concatMap showTransition trs) $ return (sys, uind) 

resetTID :: [(Transition Sigma, (TransitionID, RWSet))] -> [(Transition Sigma, (TransitionID, RWSet))] 
resetTID = reverse . snd . foldl (\(cnt,rest) l -> let (ncnt,l') = resetTID' cnt l in (ncnt,l':rest)) (0,[])

resetTID' :: Int -> (Transition Sigma, (TransitionID, RWSet)) -> (Int, (Transition Sigma, (TransitionID, RWSet)))
resetTID' c (((pid,_,act),fn),(_,annot)) = (c+1,(((pid,c,act),fn),(c,annot)))

getInitialDecls :: Decls -> LSigma
getInitialDecls = foldl (\a decl -> convertDecl decl ++ a) [] 
  where 
    convertDecl decl = case decl of
      FunctionDecl _ _ _ -> [] 
      GlobalDecl _ (Ident i) Nothing -> [(BS.pack i, Top)]
      GlobalDecl _ (Ident i@"__poet_mutex_death") (Just (Const (IntValue v))) -> [(BS.pack i, (IntVal $ fromInteger v))]
      GlobalDecl _ (Ident i) (Just (Const (IntValue v))) -> [(BS.pack i, Interval (I (fromInteger v), I (fromInteger v)))]
      GlobalDecl _ (Ident i) (Just (Call "nondet" [Const (IntValue l), Const (IntValue u)])) -> [(BS.pack i, Interval (I (fromInteger l), I (fromInteger u)))]
      GlobalDecl _ (Index (Ident i) _) _ -> [] --error "getInitialDecls: global array is not supported yet"
      _ -> error "getInitialState: not supported yet"

-- for each transition: 
-- type Transition s = (ProcessID, TransitionID, TransitionFn s)
-- process id is the name of the function
-- transition id is the position in the vector of transitions 
getTransitions :: Flow -> Definition -> [(Transition Sigma, (TransitionID, RWSet))] 
getTransitions flow (FunctionDef _ name _ stat) = recGetTrans flow (BS.pack name) stat

recGetTrans :: Flow -> ProcessID -> Statement -> [(Transition Sigma, (TransitionID, RWSet))] 
recGetTrans flow name stat =
    foldl (\acc st -> let rest = toTransition name 0 flow st
                      in acc++rest) [] stat    

-- @main encoding function
toTransition :: ProcessID -> TransitionID -> Flow -> AnnStatement PC -> [(Transition Sigma, (TransitionID, RWSet))]
toTransition procName tID flow s =
  let pcVar = BS.pack $ "pc." ++ (BS.unpack procName)
      trInfo = \act -> (procName, tID, act)
      trInfoDefault = trInfo [Other]
  in case s of
      ExprStat pc _expr ->
        case _expr of
          Call fname args ->
            let trrws = fromCall flow pcVar pc fname args
            in map (\(tr, act, rw) -> ((trInfo act, tr), (tID,rw))) trrws
          Assign _ _lhs _rhs ->
            let trrws = fromAssign flow pcVar pc _lhs _rhs
            in map (\(tr, rw) -> ((trInfoDefault, tr), (tID,rw))) trrws
      If pc _cond _then _else ->
        let trrws = fromIf flow pcVar pc _cond 
            _thentr = recGetTrans flow procName _then
            _elsetr = recGetTrans flow procName _else
            _condtr = map (\(tr, rw) -> ((trInfoDefault, tr), (tID,rw))) trrws 
        in trace ("Size of IF: " ++ show (length trrws)) $ _condtr ++ _thentr ++ _elsetr
      IfThen pc _cond _then ->
        let trrws = fromIf flow pcVar pc _cond 
            _thentr = recGetTrans flow procName _then
            _condtr = map (\(tr, rw) -> ((trInfoDefault, tr), (tID,rw))) trrws 
        in _condtr ++ _thentr
      Goto pc loc -> 
        let trrws = fromGoto flow pcVar pc
        in map (\(tr, rw) -> ((trInfoDefault, tr), (tID,rw))) trrws 
      _ -> error $ "toTransition: " ++ show s
        
modifyList :: [a] -> a -> Integer -> [a]
modifyList xs a idx = 
  let (left,_:right) = splitAt (fromInteger idx) xs
  in left ++ (a:right)
      
-- Encodes Call Statement: Same as concrete transformer
-- @Sep.'15: Support for __poet_fail, __poet_mutex_lock and __poet_mutex_unlock
fromCall :: Flow -> Var -> PC -> String -> [Expression] -> [(TransitionFn Sigma, Acts, RWSet)]
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

-- Encodes Assign Statement: same as concrete
fromAssign :: Flow -> Var -> PC -> Expression -> Expression -> [(TransitionFn Sigma, RWSet)]
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
                     Array vs = safeLookup "fromAssign" s ident
                     vs' = modifyList vs val idx
                     iVal = Array vs'
                 in [insert ident iVal s]
           else []
  in [(fn, act)]

-- Encodes Goto Statement: same as concrete
fromGoto :: Flow -> Var -> PC -> [(TransitionFn Sigma, RWSet)]
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

-- Encodes If Statement
-- @ if cond is transformed in two transitions
--   conditionals now have side effects
fromIf :: Flow -> Var -> PC -> Expression -> [(TransitionFn Sigma, RWSet)]
fromIf flow pcVar pc _cond = 
  let Branch (t,e) = getFlow flow pc
      readVars = getIdent _cond
      annots = (Write $ V pcVar):(map Read readVars)
      fnThen = \s ->
        let IntVal curPC = safeLookup "if" s pcVar
            valCond = evalCond _cond s
        in if curPC == pc
           then case valCond of
               Nothing -> [] -- the conditional is not satisfied
               Just s' ->
                 let pcVal = IntVal t
                 in [insert pcVar pcVal s']
           else []
      fnElse = \s ->
        let IntVal curPC = safeLookup "if" s pcVar
            valCond = evalCond (UnaryOp CNegOp _cond) s
        in if curPC == pc
           then case valCond of
               Nothing -> [] -- the conditional is not satisfied
               Just s' ->
                 let pcVal = IntVal e
                 in [insert pcVar pcVal s']
           else []
  in [(fnThen, annots),(fnElse, annots)]

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

-- eval arithmetic expressions
eval :: Expression -> Sigma -> Value
eval expr s = case expr of
  BinOp op lhs rhs ->
    let lhsv = eval lhs s
        rhsv = eval rhs s
    in applyArith op lhsv rhsv
  UnaryOp op rhs ->
    let v = eval rhs s
    in case op of
        CPlusOp -> v
        CMinOp  -> eval (BinOp CAddOp (Const (IntValue 0)) rhs) s
        _ -> error $ "eval: unsupported unary op: " ++ show expr    
  Const (IntValue v) -> Interval (I (fromInteger v), I (fromInteger v))
  Ident i -> 
    let ident = BS.pack i
    in safeLookup "eval" s ident
  Index (Ident i) rhs -> error "eval: arrays with intervals are not supported yet"
  Call "nondet" [Const (IntValue l), Const (IntValue u)] ->
    Interval (I (fromInteger l), I (fromInteger u))
--  Call fname args ->
{-    let ident = BS.pack i
        v = safeLookup "eval" s ident  
        vhs = eval rhs s
    in case v of
      IntVal idx -> error $ "eval: fatal error " ++ show expr
      Array vs -> case vhs of
        IntVal idx -> vs!!idx
        Array _ -> error $ "eval: disallowed " ++ show expr           
-}
  _ -> error $ "eval: disallowed " ++ show expr

-- apply arithmetic expressions
applyArith :: OpCode -> Value -> Value -> Value
applyArith op Top _ = Top
applyArith op _ Top = Top
applyArith op Bot r = r
applyArith op l Bot = l
applyArith op (Interval lhs) (Interval rhs) =
  case op of
    CAddOp -> Interval $ interval_add lhs rhs
    CSubOp -> Interval $ interval_sub lhs rhs
    CMulOp -> Interval $ interval_mult lhs rhs
    CDivOp -> Interval $ interval_div lhs rhs
    CRmdOp -> error "mod is not supported"
applyArith _ _ _ = error "apply: not all sides are intervals"
  
-- eval logical expressions
evalCond :: Expression -> Sigma -> Maybe Sigma
evalCond expr s = trace ("evaluating condition: " ++ show expr) $ case expr of
  BinOp op lhs rhs -> applyLogic s op lhs rhs
  UnaryOp CNegOp rhs ->
    let rhs' = negExp rhs
    in evalCond rhs' s
  _ -> error $ "evalCond: unsupported " ++ show expr

-- negates logical expression using De Morgan Laws
negExp :: Expression -> Expression
negExp expr = trace ("negating expression: " ++ show expr) $ case expr of
  BinOp CLndOp l r -> BinOp CLorOp (negExp l) (negExp r)
  BinOp CLorOp l r -> BinOp CLndOp (negExp l) (negExp r)
  BinOp op l r -> BinOp (negOp op) l r
  UnaryOp CNegOp e -> negExp e
  _ -> error $ "negExp: unsupported " ++ show expr

negOp :: OpCode -> OpCode
negOp op = case op of
  CLeOp -> CGeqOp
  CGrOp -> CLeqOp
  CLeqOp -> CGrOp
  CGeqOp -> CLeOp
  CEqOp -> CNeqOp
  CNeqOp -> CEqOp
  _ -> error $ "negOp: unsupported " ++ show op
  
-- apply logical operations
-- if this function returns nothing is because the condition is false
applyLogic :: Sigma -> OpCode -> Expression -> Expression -> Maybe Sigma
applyLogic s op lhs rhs = 
  let lhs_v = eval lhs s
      rhs_v = eval rhs s
  in case op of
    CLeOp  -> interval_le s lhs rhs
    CGrOp  -> interval_gr s lhs rhs
    CLeqOp -> interval_leq s lhs rhs
    CGeqOp -> interval_geq s lhs rhs
    CEqOp  -> interval_eq s lhs rhs
    CNeqOp -> interval_neq s lhs rhs
    CLndOp -> do
      r <- evalCond lhs s
      evalCond rhs r
    CLorOp -> 
      case evalCond lhs s of
        Nothing -> evalCond rhs s
        Just s' -> Just s'
apply _ _ _ = error "apply: not all sides are just integer values"

-- Logical Operations
-- Less than (CLeOp)
interval_le :: Sigma -> Expression -> Expression -> Maybe Sigma
interval_le s (Ident x_i) (Ident y_i) = 
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      y = BS.pack y_i
      y_val = safeLookup "interval_eq" s y
  in if strictly_subsumes x_val y_val
     then let y_res = interval_diff y_val x_val
          in Just $ insert y y_res s
     else Nothing
interval_le s (Ident x_i) rhs =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      rhs_val = eval rhs s
      m = interval_meet x_val rhs_val
  in if m /= Bot
     then Just s
     else Nothing
interval_le s lhs (Ident x_i) =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      lhs_val = eval lhs s --Interval (MinusInf, upperBound $ eval lhs s)
      m = interval_meet x_val lhs_val
  in if m /= Bot
     then let x_res = interval_diff x_val lhs_val
          in Just s -- $ insert x x_res s
     else Nothing
interval_le s lhs rhs = 
  let lhs_val = eval lhs s
      rhs_val = eval rhs s
  in if strictly_subsumes lhs_val rhs_val
     then Just s
     else Nothing

-- Greater than (CGrOp) 
interval_gr :: Sigma -> Expression -> Expression -> Maybe Sigma
interval_gr s lhs rhs = interval_le s rhs lhs

-- Less or Equal than (CLeqOp)
interval_leq :: Sigma -> Expression -> Expression -> Maybe Sigma
interval_leq s (Ident x_i) (Ident y_i) =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      y = BS.pack y_i
      y_val = safeLookup "interval_eq" s y
  in if subsumes x_val y_val
     then let y_res = interval_diff_eq y_val x_val
          in Just $ insert y y_res s
     else Nothing
interval_leq s (Ident x_i) rhs =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      rhs_val = Interval (MinusInf, upperBound $ eval rhs s)
      m = interval_meet x_val rhs_val
      x_res = interval_diff_eq x_val rhs_val
  in if m /= Bot --subsumes x_val rhs_val
     then Just $ insert x x_res s
     else Nothing
interval_leq s lhs (Ident x_i) =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      lhs_val = Interval (MinusInf, upperBound $ eval lhs s)
      m = interval_meet x_val lhs_val
  in if m /= Bot --subsumes lhs_val x_val
     then let x_res = interval_diff_eq x_val lhs_val
          in Just $ insert x x_res s
     else Nothing
interval_leq s lhs rhs = 
  let lhs_val = eval lhs s
      rhs_val = eval rhs s
  in if subsumes lhs_val rhs_val
     then Just s
     else Nothing


-- Greater or Equal than (CGeqOp)
interval_geq :: Sigma -> Expression -> Expression -> Maybe Sigma
interval_geq s lhs rhs = interval_leq s rhs lhs

-- Equal than (CEqOp)
interval_eq :: Sigma -> Expression -> Expression -> Maybe Sigma
interval_eq s (Ident x_i) (Ident y_i) =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      y = BS.pack y_i
      y_val = safeLookup "interval_eq" s y
      res = interval_meet x_val y_val
  in case res of
    Bot -> Nothing
    _ -> Just $ insert x res $ insert y res s
interval_eq s (Ident x_i) rhs =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      rhs_val = eval rhs s
      res = interval_meet x_val rhs_val -- Top
  in case res of
    Bot -> Nothing
    _ -> Just $ insert x res s
interval_eq s lhs (Ident x_i) =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      lhs_val = eval lhs s
      res = interval_meet x_val lhs_val
  in case res of
    Bot -> Nothing
    _ -> Just $ insert x res s
interval_eq s lhs rhs = 
  let lhs_val = eval lhs s
      rhs_val = eval rhs s
      res = interval_meet lhs_val rhs_val
  in case res of
    Bot -> Nothing
    _ -> Just s

-- Not equal than (CNeqOp)
interval_neq :: Sigma -> Expression -> Expression -> Maybe Sigma
interval_neq s (Ident x_i) (Ident y_i) =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      y = BS.pack y_i
      y_val = safeLookup "interval_eq" s y
      res = interval_meet x_val y_val
      x_res = interval_diff x_val res
      y_res = interval_diff y_val res
  in case x_res of
    Bot -> case y_res of
      Bot -> Nothing -- @assert that x_val == y_val (this should be the case by construction)
      _ -> Just $ insert y y_res s
    _ -> case y_res of 
      Bot -> Just $ insert x x_res s
      _ -> Just $ insert x x_res $ insert y y_res s
interval_neq s (Ident x_i) rhs =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      rhs_val = eval rhs s
      res = interval_meet x_val rhs_val
      x_res = interval_diff x_val res
      rhs_res = interval_diff rhs_val res
  in case x_res of
    Bot -> case rhs_res of 
      Bot -> Nothing
      _ -> Just s
    _ -> Just $ insert x x_res s
interval_neq s lhs (Ident x_i) =
  let x = BS.pack x_i
      x_val = safeLookup "interval_eq" s x
      lhs_val = eval lhs s
      res = interval_meet x_val lhs_val
      x_res = interval_diff x_val res
      lhs_res = interval_diff lhs_val res
  in case x_res of
    Bot -> case lhs_res of 
      Bot -> Nothing
      _ -> Just s
    _ -> Just $ insert x x_res s
interval_neq s lhs rhs = 
  let lhs_val = eval lhs s
      rhs_val = eval rhs s
  in if lhs_val == rhs_val
     then Nothing
     else Just s
  
