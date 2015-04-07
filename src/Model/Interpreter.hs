module Model.Interpreter where

import qualified Data.Vector as V
import qualified Data.Maybe as M
import qualified Data.ByteString.Char8 as BS

import Data.Char
import Control.Monad.ST
import System.IO.Unsafe

import Model.GCS

exec :: Int -> (System s, UIndep) -> ST s String
exec thcount (sys,indep) = execIt thcount "" indep sys (initialState sys)

execIt :: Int -> String -> UIndep -> System s -> Sigma s -> ST s String
execIt thcount str indep sys st = do
    trs <- enabledTransitions sys st
    ststr <- showSigma st
    if V.null trs
    then do
        check <- isDeadlock thcount st 
        if check
        then return $ str ++ ststr
        else error $ "exec fatal: " ++ show (thcount+1) ++ "\n" ++ show ststr
    else case trs V.!? 0 of
      Nothing -> error $ "execIt getTransition fail!"
      Just (trID,procID) -> do
        let tr = getTransitionWithID sys trID
            nstr = str ++ ststr ++ (printIndep indep $ V.toList trs) ++ "\nRunning " ++ show (trID,procID) ++ "\n"
        fn <- (tr st >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
        (nst,_) <- fn st
        execIt thcount nstr indep sys nst

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
    let s1 = unsafePerformIO $ putStrLn "current state:"
        s2 = unsafePerformIO $ putStrLn ststr
        s3 = unsafePerformIO $ putStrLn menu
        s4 = unsafePerformIO $ print trs
        s5 = unsafePerformIO $ putStrLn $ printIndep indep $ V.toList trs
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