module Model.Interpreter where

import qualified Data.Vector as V
import qualified Data.Maybe as M
import Data.Char
import Control.Monad.ST
import System.IO.Unsafe

import Model.GCS

exec :: System s -> ST s String
exec sys = execIt "" sys (initialState sys)

execIt :: String -> System s -> Sigma s -> ST s String
execIt str sys st = do
    trs <- enabledTransitions sys st
    ststr <- showSigma st
    if V.null trs
    then return $ str ++ ststr
    else case trs V.!? 0 of
      Nothing -> error $ "execIt getTransition fail!"
      Just (trID,procID) -> do
        let tr = getTransitionWithID sys trID
            nstr = str ++ ststr ++ "\nRunning " ++ show (trID,procID) ++ "\n"
        fn <- (tr st >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
        (nst,_) <- fn st
        execIt nstr sys nst
    
interpret :: System s -> ST s Int
interpret sys = interpretIt 0 sys (initialState sys)

interpretIt :: Int -> System s -> Sigma s -> ST s Int
interpretIt step sys st = do
    trs <- enabledTransitions sys st
    ststr <- showSigma st
    let s1 = unsafePerformIO $ putStrLn "current state:"
        s2 = unsafePerformIO $ putStrLn ststr
        s3 = unsafePerformIO $ putStrLn menu
        s4 = unsafePerformIO $ print trs
    if s1 `seq` s2 `seq` s3 `seq` s4 `seq` V.null trs
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
             interpretIt (step+1) sys nst
      else do 
        return step
        
menu :: String
menu = "Choose an enabled transition (by the position in the list, 'x' to quit):"
--menu = unlines ["Choose a transition by position:", "(a): automatic mode", "(n): number of enabled transition", "(x): exit"]