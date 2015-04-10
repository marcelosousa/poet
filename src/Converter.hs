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

pmdVar = BS.pack "__poet_mutex_death"
pmdVal = (IntVal 1, Just $ Var [])
pmtVar = BS.pack "__poet_mutex_threads"
pmjVar = BS.pack "__poet_mutex_threads_join"

convert :: Program -> FirstFlow -> Flow -> Int -> ST s (System s, UIndep)
convert (Program (decls, defs)) pcs flow thCount = do
  -- @ get the initial local state: this will be the set of global variables 
  --   minus the pcs
  let ils = getInitialDecls decls
      pmtiv = Array $ map IntVal $ replicate thCount 1
      pmtivl = Just $ ArrayLock $ map Var $ replicate thCount []
      ipcs = map (\(i,pc) -> (BS.pack ("pc."++i), (IntVal pc, Nothing))) pcs
      iils = ils++ipcs
      fils = (pmdVar, pmdVal):(pmtVar, (pmtiv, pmtivl)):(pmjVar, (pmtiv, pmtivl)):iils
  is <- toInitialState iils
  H.insert is pmdVar pmdVal
  H.insert is pmtVar (pmtiv, pmtivl)
  H.insert is pmjVar (pmtiv, pmtivl)
  atrs <- mapM (getTransitions flow) defs >>= return . resetTID . concat
  let (trs,annot) = unzip atrs
--      vtrs = trace ("transitions = " ++ concatMap showTransition trs ++ "\n" ++ show annot) $ V.fromList trs
      vtrs = V.fromList trs
      uind = computeUIndep annot
      sys = System vtrs is fils
  return (sys, uind)       
  --trace ("fromConvert: transitions = " ++ concatMap showTransition trs) $ return (sys, uind) 

resetTID :: [(Transition s, (TransitionID, RWSet))] -> [(Transition s, (TransitionID, RWSet))] 
resetTID = reverse . snd . foldl (\(cnt,rest) l -> let (ncnt,l') = resetTID' cnt l in (ncnt,l':rest)) (0,[])

resetTID' :: Int -> (Transition s, (TransitionID, RWSet)) -> (Int, (Transition s, (TransitionID, RWSet)))
resetTID' c ((pid,_,fn),(_,annot)) = (c+1,((pid,c,fn),(c,annot)))

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
      GlobalDecl _ (Ident i) Nothing -> [(BS.pack i, (IntVal 0, Nothing))]
      GlobalDecl _ (Ident i) (Just (IntValue v)) -> [(BS.pack i, (IntVal $ fromInteger v, Nothing))]
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
                -- @ Lock Variable
                Ident i -> do 
                  let ident = BS.pack i
                  (IntVal v, mlock) <- safeLookup "call" s ident
                  if v == 0
                  then do -- @ lock is not taken
                    let pcVal = (IntVal next, Nothing)
                        iVal = (IntVal 1, Nothing)
                    H.insert s pcVar pcVal
                    H.insert s ident iVal
                    return (s, [(pcVar, pcVal),(ident, iVal)])
                  else -- @ lock is already taken
                    case mlock of
                      Nothing -> error "should not happen"
                      Just (Var locks) -> do
                        let pcVal = (IntVal (-1), Nothing)
                            iVal = (IntVal v, Just $ Var $ (pcVar,pc):locks)
                        H.insert s pcVar pcVal
                        H.insert s ident iVal
                        return (s, [(pcVar, pcVal),(ident, iVal)]) 
                -- @ Array of Locks                   
                Index (Ident i) (Const (IntValue idx)) -> do
                  let ident = BS.pack i
                  (Array vs, mlock) <- safeLookup "call" s ident
                  let IntVal v = vs!!(fromInteger idx)
                  if v == 0
                  then do -- @ lock is not taken
                    let pcVal = (IntVal next, Nothing)
                    H.insert s pcVar pcVal
                    let vs' = modifyList vs (IntVal 1) idx
                        iVal = (Array vs', mlock)
                    H.insert s ident iVal
                    return (s, [(pcVar, pcVal),(ident, iVal)])
                  else  -- @ lock is already taken
                    case mlock of
                      Nothing -> error "should not happen"                                                    
                      Just (ArrayLock locks) -> do
                        let Var vlock = locks!!(fromInteger idx)
                            nidx = Var $ (pcVar,pc):vlock
                            locks' = modifyList locks nidx idx
                            pcVal = (IntVal (-1), Nothing)
                            iVal = (Array vs, Just $ ArrayLock locks')
                        H.insert s pcVar pcVal
                        H.insert s ident iVal
                        return (s, [(pcVar, pcVal),(ident, iVal)])
            "__poet_mutex_unlock" -> 
              case param of
                -- @ Lock Variable
                Ident i -> do 
                  let ident = BS.pack i
                  (IntVal v, mlock) <- safeLookup "call" s ident
                  if v == 0
                  then error "unlock a free lock"
                  else -- @ Lock is taken
                    case mlock of
                      Nothing -> error "should not happen"
                      Just (Var locks) -> do
                        let pcVal = (IntVal next, Nothing)
                            iVal = (IntVal 0, Just $ Var [])
                            pcValfn = \pc -> (IntVal pc, Nothing)
                        H.insert s pcVar pcVal
                        H.insert s ident iVal
                        mapM_ (\(pcVar',pc') -> H.insert s pcVar' (pcValfn pc')) locks
                        let ch = map (\(pcVar',pc') -> (pcVar', pcValfn pc')) locks
                        return (s, [(pcVar, pcVal),(ident, iVal)]++ch)                    
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
                            pcVal = (IntVal next, Nothing)
                            iVal = (Array vs', Just $ ArrayLock locks')
                            pcValfn = \pc -> (IntVal pc, Nothing)
                        H.insert s pcVar pcVal
                        H.insert s ident iVal
                        mapM_ (\(pcVar',pc') -> H.insert s pcVar' (pcValfn pc')) vlock
                        let ch = map (\(pcVar',pc') -> (pcVar', pcValfn pc')) vlock
                        return (s, [(pcVar, pcVal),(ident, iVal)]++ch)                            
            _ -> error "fromCall: call not supported"
        else return Nothing
  return [(fn, acts)] 

getVarArg :: Expression -> Var
getVarArg (Ident i) = BS.pack i
getVarArg (Index (Ident i) _) = BS.pack i
getVarArg e = error $ "getVarArg: " ++ show e

getIdent :: Expression -> [Var]
getIdent expr = case expr of
  BinOp op lhs rhs -> getIdent lhs ++ getIdent rhs
  UnaryOp op rhs -> getIdent rhs
  Const v -> []
  Ident i -> [BS.pack i]
  Index (Ident i) rhs -> (BS.pack i):getIdent rhs
  Call _ args -> concatMap getIdent args
  _ -> error $ "eval: disallowed " ++ show expr


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
                let pcVal = (IntVal next, Nothing)
                H.insert s pcVar pcVal
                val <- eval _rhs s
                case _lhs of 
                  Ident i -> do
                    let ident = BS.pack i
                        iVal = (val, Nothing)
                    H.insert s ident iVal
                    return (s, [(pcVar, pcVal),(ident, iVal)])
                  Index (Ident i) (Const (IntValue idx)) -> do
                    let ident = BS.pack i
                    (Array vs, mlock) <- safeLookup "call" s ident
                    let vs' = modifyList vs val idx
                        iVal = (Array vs', mlock)
                    H.insert s ident iVal
                    return (s, [(pcVar, pcVal),(ident, iVal)])
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
                let pcVal = (IntVal next, Nothing)
                H.insert s pcVar pcVal
                return (s, [(pcVar, pcVal)])
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
                then do
                  let pcVal = (IntVal t, Nothing)
                  H.insert s pcVar pcVal
                  return (s, [(pcVar, pcVal)])
                else do
                  let pcVal = (IntVal e, Nothing) 
                  H.insert s pcVar pcVal
                  return (s, [(pcVar, pcVal)])
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
