module Model.Interpreter where

import System.Random
import qualified Data.Vector as V
import qualified Data.Maybe as M
import qualified Data.ByteString.Char8 as BS

import Data.Char
import Control.Monad.ST
import System.IO.Unsafe

import Model.GCS

exec :: StdGen -> Int -> (System s, UIndep) -> ST s String
exec gen thcount (sys,indep) = execIt gen thcount "" indep sys (initialState sys)

execIt :: StdGen -> Int -> String -> UIndep -> System s -> Sigma s -> ST s String
execIt gen thcount str indep sys st = do
    trs <- enabledTransitions sys st
    ststr <- showSigma st
    if V.null trs
    then do
        check <- isDeadlock thcount st 
        if check
        then return $ str ++ ststr
        else error $ "exec fatal: " ++ show (thcount+1) ++ "\n" ++ show ststr
    else do 
      let ltrs = "Enabled transitions: " ++ (show $ V.toList trs)
          indepTr = getIndepTr indep $ V.toList trs
      check <- checkDiamonds sys indepTr st
      if check
      then do
        let (n, gen') = randomR (0,V.length trs - 1) gen
        case trs V.!? n of
          Nothing -> error $ "execIt getTransition fail!"
          Just (trID,procID) -> do
            let tr = getTransitionWithID sys trID
                nstr = str ++ ststr ++ ltrs ++ "\nIndependent transitions=" ++ show indepTr ++ "\nRunning " ++ show (trID,procID) ++ "\n\n"
            fn <- (tr st >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
            (nst,_) <- fn st
            execIt gen' thcount nstr indep sys nst
      else error "diamond check failed"
          
checkDiamonds :: System s -> [((TransitionID,ProcessID),(TransitionID,ProcessID))] -> Sigma s -> ST s Bool
checkDiamonds sys [] s = return True
checkDiamonds sys (((t1,_),(t2,_)):rest) s = do
    check <- checkDiamond sys t1 t2 s
    if check
    then checkDiamonds sys rest s
    else error $ "checkDiamonds: " ++ show (t1,t2)

checkDiamond :: System s -> TransitionID -> TransitionID -> Sigma s -> ST s Bool
checkDiamond sys t1 t2 s = do
    s1 <- copy s
    s2 <- copy s
    let t1fn = getTransition sys t1
        t2fn = getTransition sys t2
    -- t1(s)
    fn11 <- (t1fn s1 >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
    (s1t1,_) <- fn11 s1
    -- t2(t1(s))
    fn21 <- (t2fn s1t1 >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
    (s12,_) <- fn21 s1t1
    -- t2(s)
    fn22 <- (t2fn s2 >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
    (s2t2,_) <- fn22 s2
    -- t1(t2(s))
    fn12 <- (t1fn s2t2 >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
    (s22,_) <- fn12 s2t2
    equals s12 s22
            
isDeadlock :: Int -> Sigma s -> ST s Bool
isDeadlock thcount st = do
    let ident = BS.pack "__poet_mutex_death"
    (_, mlock) <- safeLookup "isDeadlock" st ident
    case mlock of
        Just (Var locks) -> return $ length locks == (thcount+1)
        _ -> error $ "isDeadlock: " ++ show mlock
        
interpret :: (System s, UIndep) -> ST s Int
interpret (sys,indep) = interpretIt 0 indep sys (initialState sys)

interpretIt :: Int -> UIndep -> System s -> Sigma s -> ST s Int
interpretIt step indep sys st = do
    trs <- enabledTransitions sys st
    ststr <- showSigma st
    let indepTr = getIndepTr indep $ V.toList trs
        s1 = unsafePerformIO $ putStrLn "current state:"
        s2 = unsafePerformIO $ putStrLn ststr
        s3 = unsafePerformIO $ putStrLn menu
        s4 = unsafePerformIO $ print trs
        s5 = unsafePerformIO $ putStrLn $ "independent transitions=" ++ show indepTr
    if s1 `seq` s2 `seq` s3 `seq` s4 `seq` s5 `seq` V.null trs
    then do
      let f = unsafePerformIO $ print "Finished execution"
      f `seq` return step  
    else do
      let c = unsafePerformIO $ getChar
          n = case c of
           'a' -> 0
           'x' -> (-1)
           _ -> digitToInt c
      if n >= 0
      then do
         case trs V.!? n of
           Nothing -> error $ "interpret getTransition fail: " ++ show n
           Just (trID,_) -> do
             let tr = getTransitionWithID sys trID
             fn <- (tr st >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
             (nst,_) <- fn st
             interpretIt (step+1) indep sys nst
      else do 
        return step
        
menu :: String
menu = "Choose an enabled transition (by the position in the list, 'x' to quit):"
--menu = unlines ["Choose a transition by position:", "(a): automatic mode", "(n): number of enabled transition", "(x): exit"]