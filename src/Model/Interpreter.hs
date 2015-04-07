module Model.Interpreter where

import qualified Data.Vector as V
import qualified Data.Maybe as M
import Data.Char
import Control.Monad.ST
import System.IO.Unsafe

import Model.GCS

interpreter :: System s -> ST s Int
interpreter sys = interpret 0 sys (initialState sys)

interpret :: Int -> System s -> Sigma s -> ST s Int
interpret step sys st = do
    trs <- enabledTransitions sys st
    ststr <- showSigma st
    let s1 = unsafePerformIO $ print "current state:"
        s2 = unsafePerformIO $ putStrLn ststr
        s3 = unsafePerformIO $ print "enabled transitions:"
        s4 = unsafePerformIO $ print trs
    if s1 `seq` s2 `seq` s3 `seq` s4 `seq` V.null trs
    then do
      return $ unsafePerformIO $ print "Finished execution"
      return step  
    else do
      return $ unsafePerformIO $ print menu
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
             let tr = getTransition sys trID
             fn <- (tr st >>= return . M.fromMaybe (error $ "newState: the transition was not enabled"))
             (nst,_) <- fn st
             interpret (step+1) sys nst
      else do 
        return $ unsafePerformIO $ print "Exit."
        return step
        
menu :: String
menu = unlines ["Choose an option:", "(a): automatic mode", "(n): number of enabled transition", "(x): exit"]
