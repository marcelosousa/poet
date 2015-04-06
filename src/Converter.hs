module Converter where

import Data.Maybe
import Control.Monad.ST.Safe
import Control.Monad
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H

import Language.SimpleC.AST hiding (Value)
import Language.C.Syntax.AST (CBinaryOp(..),CUnaryOp(..))
import Model.GCS
import Frontend
import Frontend.Util

import Debug.Trace

-- read write data type
data RW = Read Var | Write Var
  deriving (Show,Eq,Ord)

type RWSet = [RW]

convert :: Program -> FirstFlow -> Flow -> Int -> ST s (System s, UIndep)
convert (Program (decls, defs)) pcs flow thCount = do
  -- @ get the initial local state: this will be the set of global variables 
  --   minus the pcs
  let ils = getInitialDecls decls
      pmtiv = Array $ map IntVal $ replicate (thCount+1) 1
      pmtivl = Just $ ArrayLock $ map Var $ replicate (thCount+1) []
      ipcs = map (\(i,pc) -> (BS.pack ("pc."++i), IntVal pc)) pcs
  is <- toInitialState $ ils++ipcs
  H.insert is (BS.pack "__poet_mutex_death") (IntVal 1, Just $ Var [])
  H.insert is (BS.pack "__poet_mutex_threads") (pmtiv, pmtivl)
  (trs,annot) <- mapM (getTransitions flow) defs >>= return . unzip . concat
  -- @ complete the initial state with the pcs: put the pc = 1 for main (later on put this on a user defined function)
  let vtrs = V.fromList trs
      uind = computeUIndep (map fst ils) annot
      sys = System vtrs is ils  
  return (sys, uind) 

computeUIndep :: [Var] -> [(TransitionID, RWSet)] -> UIndep
computeUIndep globals = undefined

getInitialDecls :: Decls -> LSigma
getInitialDecls = foldl (\a decl -> convertDecl decl ++ a) [] 
  where 
    convertDecl decl = case decl of
      FunctionDecl _ _ _ -> [] 
      GlobalDecl _ (Ident i) Nothing -> [(BS.pack i, IntVal 0)]
      GlobalDecl _ (Ident i) (Just (IntValue v)) -> [(BS.pack i, IntVal $ fromInteger v)]
      GlobalDecl _ (Index (Ident i) _) _ -> error "getInitialDecls: global array is not supported yet"
      _ -> error "getInitialState: not supported yet"

-- @ computeInitialState 
toInitialState :: LSigma -> ST s (ISigma s)
toInitialState lst = do
  ht <- H.new
  mapM_ (\(n,v) -> H.insert ht n (v,Nothing)) lst
  return ht  

-- for each transition: 
-- type Transition s = (ProcessID, TransitionID, TransitionFn s)
-- process id is the name of the function
-- transition id is the position in the vector of transitions 
getTransitions :: Flow -> Definition -> ST s [(Transition s, (TransitionID, RWSet))] 
getTransitions flow (FunctionDef _ name _ stat) = recGetTrans flow (BS.pack name) stat

recGetTrans :: Flow -> ProcessID -> Statement -> ST s [(Transition s, (TransitionID, RWSet))] 
recGetTrans flow name stat =
    foldM (\acc st -> toTransition name 0 flow st >>= \rest -> return $ acc++rest) [] stat    

toTransition :: ProcessID -> TransitionID -> Flow -> AnnStatement PC -> ST s [(Transition s, (TransitionID, RWSet))]
toTransition procName tID flow s = 
    let pcVar = BS.pack $ "pc." ++ (BS.unpack procName)
    in case s of
        ExprStat pc _expr ->
          case _expr of
            Call fname args -> do
                trrws  <- fromCall flow pcVar pc fname args
                return $ map (\(tr, rw) -> ((procName, tID, tr), (tID,rw))) trrws
            Assign _ _lhs _rhs -> do
                trrws  <- fromAssign flow pcVar pc _lhs _rhs
                return $ map (\(tr, rw) -> ((procName, tID, tr), (tID,rw))) trrws
        If pc _cond _then _else -> do
            trrws <- fromIf flow pcVar pc _cond 
            _thentr <- recGetTrans flow procName _then
            _elsetr <- recGetTrans flow procName _else
            let _condtr = map (\(tr, rw) -> ((procName, tID, tr), (tID,rw))) trrws 
            return $ _condtr ++ _thentr ++ _elsetr
        IfThen pc _cond _then -> do
            trrws <- fromIf flow pcVar pc _cond 
            _thentr <- recGetTrans flow procName _then
            let _condtr = map (\(tr, rw) -> ((procName, tID, tr), (tID,rw))) trrws 
            return $ _condtr ++ _thentr
        Goto pc loc -> do
            trrws  <- fromGoto flow pcVar pc
            return $ map (\(tr, rw) -> ((procName, tID, tr), (tID,rw))) trrws 

modifyList :: [a] -> a -> Integer -> [a]
modifyList xs a idx = 
  let (left,_:right) = splitAt (fromInteger idx) xs
  in left ++ (a:right)
  
-- encodes Call
fromCall :: Flow -> Var -> PC -> String -> [Expression] -> ST s [(TransitionFn s, RWSet)]
fromCall flow pcVar pc name [param] = do
  let Continue next = getFlow flow pc
      argVar = getVarArg param
      acts = [Write pcVar, Write argVar]
      fn = \s -> do
        (IntVal curPC,_) <- safeLookup "call" s pcVar
        if curPC == pc
        then return $ Just $ \s ->
          case name of
            "__poet_mutex_lock" ->
              case param of
                Ident i -> do 
                  let ident = BS.pack i
                  (IntVal v, mlock) <- safeLookup "call" s ident
                  if v == 0
                  then do
                    H.insert s pcVar (IntVal next, Nothing)
                    H.insert s ident (IntVal 1, Nothing)
                    return (s, [(pcVar, IntVal next),(ident, IntVal 1)])
                  else 
                    case mlock of
                      Nothing -> error "should not happen"
                      Just (Var locks) -> do
                        H.insert s pcVar (IntVal (-1), Nothing)  
                        H.insert s ident (IntVal v, Just $ Var $ (pcVar,pc):locks)  
                        return (s, [(pcVar, IntVal (-1)),(ident, IntVal 1)])                    
                Index (Ident i) (Const (IntValue idx)) -> do
                  let ident = BS.pack i
                  (Array vs, mlock) <- safeLookup "call" s ident
                  let IntVal v = vs!!(fromInteger idx)
                  if v == 0
                  then do
                    H.insert s pcVar (IntVal next, Nothing)
                    let vs' = modifyList vs (IntVal 1) idx
                    H.insert s ident (Array vs', Nothing)
                    return (s, [(pcVar, IntVal next),(ident, Array vs')])
                  else 
                    case mlock of
                      Nothing -> error "should not happen"                                                    
                      Just (ArrayLock locks) -> do
                        let Var vlock = locks!!(fromInteger idx)
                            nidx = Var $ (pcVar,pc):vlock
                            locks' = modifyList locks nidx idx
                        H.insert s pcVar (IntVal (-1), Nothing)
                        H.insert s ident (Array vs, Just $ ArrayLock locks')
                        return (s, [(pcVar, IntVal next),(ident, Array vs)])
            "__poet_mutex_unlock" -> 
              case param of
                Ident i -> do 
                  let ident = BS.pack i
                  (IntVal v, mlock) <- safeLookup "call" s ident
                  if v == 0
                  then error "unlock a free lock"
                  else 
                    case mlock of
                      Nothing -> error "should not happen"
                      Just (Var locks) -> do
                        H.insert s pcVar (IntVal next, Nothing)  
                        H.insert s ident (IntVal 0, Nothing)
                        mapM_ (\(pcVar',pc') -> H.insert s pcVar' (IntVal pc',Nothing)) locks
                        let ch = map (\(pcVar',pc') -> (pcVar', IntVal pc')) locks
                        return (s, [(pcVar, IntVal next),(ident, IntVal 1)]++ch)                    
                Index (Ident i) (Const (IntValue idx)) -> do
                  let ident = BS.pack i
                  (Array vs, mlock) <- safeLookup "call" s ident
                  let IntVal v = vs!!(fromInteger idx)
                  if v == 0
                  then error "unlock a free lock"
                  else 
                    case mlock of
                      Nothing -> error "should not happen"
                      Just (ArrayLock locks) -> do
                        let Var vlock = locks!!(fromInteger idx)
                            vs' = modifyList vs (IntVal 0) idx
                            nidx = Var []
                            locks' = modifyList locks nidx idx
                        H.insert s pcVar (IntVal next, Nothing)  
                        H.insert s ident (Array vs, Just $ ArrayLock locks')
                        mapM_ (\(pcVar',pc') -> H.insert s pcVar' (IntVal pc',Nothing)) vlock
                        let ch = map (\(pcVar',pc') -> (pcVar', IntVal pc')) vlock
                        return (s, [(pcVar, IntVal next),(ident, Array vs')]++ch)                            
            _ -> error "fromCall: call not supported"
        else return Nothing
  return [(fn, acts)] 

getVarArg :: Expression -> Var
getVarArg (Ident i) = BS.pack i
getVarArg (Index (Ident i) _) = BS.pack i
getVarArg e = error $ "getVarArg: " ++ show e

getIdent :: Expression -> [Var]
getIdent = undefined

-- encodes Assign
fromAssign :: Flow -> Var -> PC -> Expression -> Expression -> ST s [(TransitionFn s, RWSet)]
fromAssign flow pcVar pc _lhs _rhs = do
    let Continue next = getFlow flow pc
        _lhsi = map Write $ getIdent _lhs
        _rhsi = map Read $ getIdent _rhs
        act = (Write pcVar):(_lhsi ++ _rhsi)
        fn = \s -> do
            (IntVal curPC,_) <- safeLookup "goto" s pcVar
            if curPC == pc
            then return $ Just $ \s -> do
                H.insert s pcVar (IntVal next, Nothing)
                val <- eval _rhs s
                case _lhs of 
                  Ident i -> do
                    let ident = BS.pack i
                    H.insert s ident (val, Nothing)
                    return (s, [(pcVar, IntVal next),(ident, val)])
                  Index (Ident i) (Const (IntValue idx)) -> do
                    let ident = BS.pack i
                    (Array vs, mlock) <- safeLookup "call" s ident
                    let vs' = modifyList vs val idx
                    H.insert s ident (Array vs', mlock)
                    return (s, [(pcVar, IntVal next),(ident, Array vs')])
            else return Nothing
    return [(fn, act)]

-- encodes GOTO
fromGoto :: Flow -> Var -> PC -> ST s [(TransitionFn s, RWSet)]
fromGoto flow pcVar pc = do
    let Continue next = getFlow flow pc
        fn = \s -> do
            (IntVal curPC,_) <- safeLookup "goto" s pcVar
            if curPC == pc
            then return $ Just $ \s -> do
                H.insert s pcVar (IntVal next, Nothing)
                return (s, [(pcVar, IntVal next)])
            else return Nothing
    return [(fn, [Write pcVar])]

-- encodes fromIf
fromIf :: Flow -> Var -> PC -> Expression -> ST s [(TransitionFn s, RWSet)]
fromIf flow pcVar pc _cond = do
    let Branch (t,e) = getFlow flow pc
        fn = \s -> do
            (IntVal curPC,_) <- safeLookup "if" s pcVar
            if curPC == pc
            then return $ Just $ \s -> do
                valCond <- evalCond _cond s 
                if valCond
                then return (s, [(pcVar, IntVal t)])
                else return (s, [(pcVar, IntVal e)])
            else return Nothing
    return [(fn, [Write pcVar])]

eval :: Expression -> Sigma s -> ST s Value
eval expr s = case expr of
  BinOp op lhs rhs -> do
    lhsv <- eval lhs s
    rhsv <- eval rhs s
    return $ apply op lhsv rhsv
  UnaryOp op rhs -> do
      v <- eval rhs s
      case op of
        CPlusOp -> return v
        CMinOp  -> case v of
          IntVal iv -> return $ IntVal (-iv)
          _ -> error $ "eval: unaryop " ++ show expr
        CNegOp  ->   case v of
          IntVal 0 -> return $ IntVal 1
          IntVal 1 -> return $ IntVal 0
          _ -> error $ "eval: unaryop " ++ show expr
        _ -> error $ "eval: disallowed unary op: " ++ show expr    
  Const (IntValue v) -> return $ IntVal $ fromInteger v
  Ident i -> do
    let ident = BS.pack i
    (v,_) <- safeLookup "eval" s ident
    return v
  Index (Ident i) rhs -> do
    let ident = BS.pack i
    (v,_) <- safeLookup "eval" s ident  
    vhs <- eval rhs s
    case v of
      IntVal idx -> error $ "eval: fatal error " ++ show expr
      Array vs -> case vhs of
        IntVal idx -> return $ vs!!idx
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

toBool :: Int -> Bool
toBool 0 = False
toBool 1 = True
toBool x = error $ "toBool: " ++ show x

fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

evalCond :: Expression -> Sigma s -> ST s Bool
evalCond expr s = do
  v <- eval expr s
  case v of
    IntVal 0 -> return False
    IntVal 1 -> return True
    _ -> error $ "evalCond: " ++ show expr

