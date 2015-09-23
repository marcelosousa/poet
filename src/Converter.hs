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
import Domain.Concrete
import Util.Generic

pmdVar = BS.pack "__poet_mutex_death"
pmdVal = IntVal 1
pmtVar = BS.pack "__poet_mutex_threads"
pmjVar = BS.pack "__poet_mutex_threads_join"

convert :: Program -> FirstFlow -> Flow -> Int -> ST s (System s, UIndep)
convert (Program (decls, defs)) pcs flow thCount = do
  -- @ get the initial local state: this will be the set of global variables 
  --   minus the pcs
  let ils = getInitialDecls decls
      pmtiv = Array $ map IntVal $ replicate thCount 1
      ipcs = map (\(i,pc) -> (BS.pack ("pc."++i), IntVal pc)) pcs
      iils = ils++ipcs
      fils = (pmdVar, pmdVal):(pmtVar, pmtiv):(pmjVar, pmtiv):iils
  is <- toInitialState iils
  H.insert is pmdVar pmdVal
  H.insert is pmtVar pmtiv
  H.insert is pmjVar pmtiv
  atrs <- mapM (getTransitions flow) defs >>= return . resetTID . concat
  let (trs,annot) = unzip atrs
--      vtrs = trace ("transitions = " ++ concatMap showTransition trs ++ "\n" ++ show annot) $ V.fromList trs
      vtrs = V.fromList trs
      uind = computeUIndep annot
      sys = System vtrs is $ (Lock (V pmdVar)):[Lock (A pmtVar (toInteger th)) | th <- [0 .. thCount-1]] ++ [Lock (A pmjVar (toInteger th)) | th <- [0 .. thCount-1]]
  return (sys, uind)       
  --trace ("fromConvert: transitions = " ++ concatMap showTransition trs) $ return (sys, uind) 

resetTID :: [(Transition s, (TransitionID, RWSet))] -> [(Transition s, (TransitionID, RWSet))] 
resetTID = reverse . snd . foldl (\(cnt,rest) l -> let (ncnt,l') = resetTID' cnt l in (ncnt,l':rest)) (0,[])

resetTID' :: Int -> (Transition s, (TransitionID, RWSet)) -> (Int, (Transition s, (TransitionID, RWSet)))
resetTID' c ((pid,_,act,fn),(_,annot)) = (c+1,((pid,c,act,fn),(c,annot)))

computeUIndep :: [(TransitionID, RWSet)] -> UIndep
computeUIndep rwsets = 
    let size = length rwsets
    in V.generate size (\i -> V.generate size (\j -> check rwsets i j))

check :: [(TransitionID, RWSet)] -> Int -> Int -> Bool
check rwsets i j = 
    let (_,tr1) = rwsets!!i
        (_,tr2) = rwsets!!j
    in not $ isRWDependent tr1 tr2

isRWDependent :: RWSet -> RWSet -> Bool
isRWDependent [] _ = False
isRWDependent ((Read v):rw1) rw2 = 
    any (\el -> el == Write v) rw2 || isRWDependent rw1 rw2
isRWDependent ((Write v):rw1) rw2 =
    any (\el -> el == Write v || el == Read v) rw2 || isRWDependent rw1 rw2

getInitialDecls :: Decls -> LSigma
getInitialDecls = foldl (\a decl -> convertDecl decl ++ a) [] 
  where 
    convertDecl decl = case decl of
      FunctionDecl _ _ _ -> [] 
      GlobalDecl _ (Ident i) Nothing -> [(BS.pack i, IntVal 0)]
      GlobalDecl _ (Ident i) (Just (IntValue v)) -> [(BS.pack i, (IntVal $ fromInteger v))]
      GlobalDecl _ (Index (Ident i) _) _ -> [] --error "getInitialDecls: global array is not supported yet"
      _ -> error "getInitialState: not supported yet"

-- @ computeInitialState 
toInitialState :: LSigma -> ST s (ISigma s)
toInitialState lst = do
  ht <- H.new
  mapM_ (\(n,v) -> H.insert ht n v) lst
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
                return $ map (\(tr, act, rw) -> ((procName, tID, act, tr), (tID,rw))) trrws
            Assign _ _lhs _rhs -> do
                trrws  <- fromAssign flow pcVar pc _lhs _rhs
                return $ map (\(tr, rw) -> ((procName, tID, [Other], tr), (tID,rw))) trrws
        If pc _cond _then _else -> do
            trrws <- fromIf flow pcVar pc _cond 
            _thentr <- recGetTrans flow procName _then
            _elsetr <- recGetTrans flow procName _else
            let _condtr = map (\(tr, rw) -> ((procName, tID, [Other], tr), (tID,rw))) trrws 
            return $ _condtr ++ _thentr ++ _elsetr
        IfThen pc _cond _then -> do
            trrws <- fromIf flow pcVar pc _cond 
            _thentr <- recGetTrans flow procName _then
            let _condtr = map (\(tr, rw) -> ((procName, tID, [Other], tr), (tID,rw))) trrws 
            return $ _condtr ++ _thentr
        Goto pc loc -> do
            trrws  <- fromGoto flow pcVar pc
            return $ map (\(tr, rw) -> ((procName, tID, [Other], tr), (tID,rw))) trrws 
        _ -> error $ "toTransition: " ++ show s
modifyList :: [a] -> a -> Integer -> [a]
modifyList xs a idx = 
  let (left,_:right) = splitAt (fromInteger idx) xs
  in left ++ (a:right)
  
-- encodes Call
fromCall :: Flow -> Var -> PC -> String -> [Expression] -> ST s [(TransitionFn s, Acts, RWSet)]
fromCall flow pcVar pc "__poet_fail" [] = do
  let acts = [Write (V pcVar)]
      fn = \s -> do
             IntVal curPC <- safeLookup "call" s pcVar
             if curPC == pc
             then return $ Just $ \s -> error "poet found an assertion violation!"
             else return Nothing
  return [(fn, [Other], acts)]
fromCall flow pcVar pc name [param] = do
  let Continue next = getFlow flow pc
  case name of 
    "__poet_mutex_lock" ->
      case param of
        -- @ Lock Variable
        Ident i -> do 
          let ident = BS.pack i
              acts = [Write (V pcVar), Write (V ident)]
              act = [Lock $ V ident]
              fn = \s -> do
                IntVal curPC <- safeLookup "call" s pcVar
                IntVal v <- safeLookup "call" s ident
                if curPC == pc && v == 0
                then return $ Just $ \s -> do
                  let pcVal = IntVal next
                      iVal = IntVal 1
                  H.insert s pcVar pcVal
                  H.insert s ident iVal
                  return s
                else return Nothing
          return [(fn, act, acts)]
        -- @ Array of Locks              
        Index (Ident i) (Const (IntValue idx)) -> do
          let ident = BS.pack i
              acts = [Write (V pcVar), Write (A ident idx)]
              act = [Lock $ A ident idx]              
              fn = \s -> do 
                IntVal curPC <- safeLookup "call" s pcVar
                Array vs <- safeLookup "call" s ident
                let IntVal v = vs!!(fromInteger idx)
                if curPC == pc && v == 0
                then return $ Just $ \s -> do
                  let pcVal = IntVal next
                      vs' = modifyList vs (IntVal 1) idx
                      iVal = Array vs'
                  H.insert s pcVar pcVal
                  H.insert s ident iVal
                  return s
                else return Nothing
          return [(fn, act, acts)]         
        Index (Ident i) (Ident idxident) -> do
          let ident = BS.pack i
              idxi = BS.pack idxident
              acts = [Write (V pcVar), Write (V ident), Read (V idxi)]
              act = [Lock $ V ident]              
              fn = \s -> do 
                IntVal curPC <- safeLookup "call lock array pc" s pcVar
                Array vs <- safeLookup ("call lock array: " ++ show ident) s ident
                IntVal idx <- safeLookup "call lock array ident" s idxi
                let IntVal v = vs!!idx
                if curPC == pc && v == 0
                then return $ Just $ \s -> do
                  let pcVal = IntVal next
                      vs' = modifyList vs (IntVal 1) (toInteger idx)
                      iVal = Array vs'
                  H.insert s pcVar pcVal
                  H.insert s ident iVal
                  return s
                else return Nothing
          return [(fn, act, acts)]         
    "__poet_mutex_unlock" ->
      case param of
        -- @ Lock Variable
        Ident i -> do 
          let ident = BS.pack i
              acts = [Write (V pcVar), Write (V ident)]
              act = [Unlock $ V ident]
              fn = \s -> do
                IntVal curPC <- safeLookup "call" s pcVar
                if curPC == pc
                then return $ Just $ \s -> do
                  let pcVal = IntVal next
                      iVal = IntVal 0
                  H.insert s pcVar pcVal
                  H.insert s ident iVal
                  return s
                else return Nothing
          return [(fn, act, acts)]
        -- @ Array of Locks
        Index (Ident i) (Const (IntValue idx)) -> do
          let ident = BS.pack i
              acts = [Write (V pcVar), Write (A ident idx)]
              act = [Unlock $ A ident idx]
              fn = \s -> do
                IntVal curPC <- safeLookup "call" s pcVar
                if curPC == pc
                then return $ Just $ \s -> do
                  IntVal curPC <- safeLookup "call" s pcVar
                  Array vs <- safeLookup "call" s ident
                  let pcVal = IntVal next
                      vs' = modifyList vs (IntVal 0) idx
                      iVal = Array vs'
                  H.insert s pcVar pcVal
                  H.insert s ident iVal
                  return s
                else return Nothing
          return [(fn, act, acts)]     
        Index (Ident i) (Ident idxident) -> do
          let ident = BS.pack i
              idxi = BS.pack idxident
              acts = [Write (V pcVar), Write (V ident), Read (V idxi)]
              act = [Unlock $ V ident]
              fn = \s -> do
                IntVal curPC <- safeLookup "call" s pcVar
                if curPC == pc
                then return $ Just $ \s -> do
                  IntVal curPC <- safeLookup "call unlock array" s pcVar
                  Array vs <- safeLookup "call unlock array " s ident
                  IntVal idx <- safeLookup "call unlock array ident" s idxi
                  let pcVal = IntVal next
                      vs' = modifyList vs (IntVal 0) (toInteger idx)
                      iVal = Array vs'
                  H.insert s pcVar pcVal
                  H.insert s ident iVal
                  return s
                else return Nothing
          return [(fn, act, acts)]                 
    _ -> error "fromCall: call not supported"

getVarArg :: Expression -> Var
getVarArg (Ident i) = BS.pack i
getVarArg (Index (Ident i) _) = BS.pack i
getVarArg e = error $ "getVarArg: " ++ show e

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


-- encodes Assign
fromAssign :: Flow -> Var -> PC -> Expression -> Expression -> ST s [(TransitionFn s, RWSet)]
fromAssign flow pcVar pc _lhs _rhs = do
    let Continue next = getFlow flow pc
        _lhsi = map Write $ getIdent _lhs
        _rhsi = map Read $ getIdent _rhs
        act = (Write $ V pcVar):(_lhsi ++ _rhsi)
        fn = \s -> do
            IntVal curPC <- safeLookup "goto" s pcVar
            if curPC == pc
            then return $ Just $ \s -> do
                let pcVal = IntVal next
                H.insert s pcVar pcVal
                val <- eval _rhs s
                case _lhs of 
                  Ident i -> do
                    let ident = BS.pack i
                        iVal = val
                    H.insert s ident iVal
                    return s
                  Index (Ident i) (Const (IntValue idx)) -> do
                    let ident = BS.pack i
                    Array vs <- safeLookup "call" s ident
                    let vs' = modifyList vs val idx
                        iVal = Array vs'
                    H.insert s ident iVal
                    return s
            else return Nothing
    return [(fn, act)]

-- encodes GOTO
fromGoto :: Flow -> Var -> PC -> ST s [(TransitionFn s, RWSet)]
fromGoto flow pcVar pc = do
    let Continue next = getFlow flow pc
        fn = \s -> do
            IntVal curPC <- safeLookup "goto" s pcVar
            if curPC == pc
            then return $ Just $ \s -> do
                let pcVal = IntVal next
                H.insert s pcVar pcVal
                return s
            else return Nothing
    return [(fn, [Write $ V pcVar])]


-- encodes fromIf
fromIf :: Flow -> Var -> PC -> Expression -> ST s [(TransitionFn s, RWSet)]
fromIf flow pcVar pc _cond = do
    let Branch (t,e) = getFlow flow pc
        readVars = getIdent _cond
        annots = (Write $ V pcVar):(map Read readVars)
        fn = \s -> do
            IntVal curPC <- safeLookup "if" s pcVar
            if curPC == pc
            then return $ Just $ \s -> do
                valCond <- evalCond _cond s 
                if valCond
                then do
                  let pcVal = IntVal t
                  H.insert s pcVar pcVal
                  return s
                else do
                  let pcVal = IntVal e
                  H.insert s pcVar pcVal
                  return s
            else return Nothing
    return [(fn, annots)]

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
    v <- safeLookup "eval" s ident
    return v
  Index (Ident i) rhs -> do
    let ident = BS.pack i
    v <- safeLookup "eval" s ident  
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
