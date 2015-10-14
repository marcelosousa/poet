module Model.Interpreter where

import Control.Monad.ST
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import qualified Data.Maybe as M
import qualified Data.Vector as V
import Model.GCS
import Model.Independence
import System.IO.Unsafe
import System.Random

exec :: (Show st, Ord st) => StdGen -> Int -> (System st, UIndep) -> String
exec gen thcount (sys,indep) = execIt gen thcount "" indep sys (initialState sys)

execIt :: (Show st, Ord st) => StdGen -> Int -> String -> UIndep -> System st -> st -> String
execIt gen thcount str indep sys st =
  let trs = enabledTransitions sys st
      ststr = show st
  in if V.null trs
     then 
       if isDeadlock thcount st
       then str ++ ststr
       else error $ "exec fatal: " ++ show (thcount+1) ++ "\n" ++ show ststr
     else 
      let ltrs = "Enabled transitions: " ++ (show $ V.toList trs)
          indepTr = getIndepTr indep $ V.toList trs
      in if checkDiamonds sys indepTr st
         then
           let (n, gen') = randomR (0,V.length trs - 1) gen
           in case trs V.!? n of
             Nothing -> error $ "execIt getTransition fail!"
             Just (procID,trID,_) ->
               let tr = getTransitionWithID sys trID
                   nstr = str ++ ststr ++ ltrs ++ "\nIndependent transitions=" ++ show indepTr ++ "\nRunning " ++ show (trID,procID) ++ "\n\n"
                   nst = head $ tr st
               in execIt gen' thcount nstr indep sys nst
      else error "diamond check failed"
          
checkDiamonds :: Ord st => System st -> [(TransitionInfo,TransitionInfo)] -> st -> Bool
checkDiamonds sys [] s = True
checkDiamonds sys (((_,t1,_),(_,t2,_)):rest) s =
  if checkDiamond sys t1 t2 s
  then checkDiamonds sys rest s
  else error $ "checkDiamonds: " ++ show (t1,t2)

checkDiamond :: Ord st => System st -> TransitionID -> TransitionID -> st -> Bool
checkDiamond sys t1 t2 s =
    let t1fn = getTransition sys t1
        t2fn = getTransition sys t2
        s1s2 = sort $ map t1fn $ t2fn s
        s2s1 = sort $ map t2fn $ t1fn s
    in s1s2 == s2s1     
            
isDeadlock :: Int -> st -> Bool
isDeadlock thcount st =  True 
{-
FIXME: Check that all pcs are in -1
do
    let ident = BS.pack "__poet_mutex_death"
    _ <- safeLookup "isDeadlock" st ident
    case mlock of
        Just (Var locks) -> return $ length locks == (thcount+1)
        _ -> error $ "isDeadlock: " ++ show mlock
 -}       

interpret :: (Show st, Ord st) => (System st, UIndep) -> Int
interpret (sys,indep) = interpretIt 0 indep sys (initialState sys)

interpretIt :: Show st => Int -> UIndep -> System st -> st -> Int
interpretIt step indep sys st =
  let trs = enabledTransitions sys st
      ststr = show st
      indepTr = getIndepTr indep $ V.toList trs
      s1 = unsafePerformIO $ putStrLn "\n========================================\nCurrent state:"
      s2 = unsafePerformIO $ putStrLn ststr
      s3 = unsafePerformIO $ putStrLn menu
      s4 = unsafePerformIO $ print trs
      s5 = unsafePerformIO $ putStrLn $ "\nIndependent transitions: " ++ show indepTr
  in if s1 `seq` s2 `seq` s3 `seq` s4 `seq` s5 `seq` V.null trs
     then
       let f = unsafePerformIO $ print "Finished execution"
       in f `seq` step
     else
       let c = unsafePerformIO $ getChar
           n = case c of
             'a' -> 0
             'x' -> (-1)
             _ -> digitToInt c
       in if n >= 0
          then
            case trs V.!? n of
              Nothing -> error $ "interpret getTransition fail: " ++ show n
              Just (_,trID,_) ->
                let tr = getTransitionWithID sys trID
                    nst = head $ tr st
                in interpretIt (step+1) indep sys nst
          else step

menu :: String
menu = "\nChoose an enabled transition (by the position in the list, 'x' to quit):"
--menu = unlines ["Choose a transition by position:", "(a): automatic mode", "(n): number of enabled transition", "(x): exit"]
