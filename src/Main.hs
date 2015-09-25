{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------

module Main where

import Control.Monad.ST
import Control.Monad.ST.Safe

import System.Console.CmdArgs
import System.FilePath.Posix
import System.Random
--import Data.Time

import Language.SimpleC
import Frontend (frontEnd)
import Domain.Concrete.Type
import Domain.Concrete.Converter
import Domain.Concrete.Interpreter
import qualified Exploration.UNF.Unfolder as Exp
import Exploration.UNF.APIStateless hiding (execute)
import Util.Printer

import Test.HUnit (runTestTT)
import Test.Tests
import Test.Examples

--import Unfolderful
--import Printer
--import Benchmark
--import Model.GCS
--import Frontend.PetriNet
--import qualified Data.Map as M
--import Control.Monad.ST.Safe


_program, _summary :: String
_summary = unlines ["POET - v0.1","Partial Order Exploration Tools is a set of exploration methods for concurrent C programs."
                   ,"Copyright 2015 @ Marcelo Sousa"]
_program = "poet"
_help    = "The input files of poet are C files written in a restricted subset of the C language. For more info, check the documentation!"
_helpFE = unlines ["poet frontend receives a concurrent C program in a restricted subset of the language and performs a series of transformations to simplify the analysis"
                  ,"Example: poet frontend file.c"]
_helpInter = unlines ["poet interpret receives a concurrent C program in a restricted subset of the language and runs the interpreter on the corresponding model of computation"
                  ,"Example: poet interpret -i=file.c"]
_helpExec = unlines ["poet interpret receives a concurrent C program in a restricted subset of the language and executes one run of the corresponding model of computation"
                  ,"Example: poet execute -i=file.c -s=int (optional)"]                 
_helpExplore = unlines ["poet explore receives a concurrent C program in a restricted subset of the language and explores the state space of the corresponding model of computation using a partial order representation"
                  ,"Example: poet explore -i=file.c"]
_helpDebug = unlines ["poet debug receives a concurrent C program in a restricted subset of the language and explores the state space and explores the unfolding. At the end, it prints the LPES."]
_helpTest = unlines ["poet test runs the explore mode over the set of examples in Test/Examples"]
                   
data Option = Frontend {input :: FilePath}
            | Middleend {input :: FilePath}
            | Execute {input :: FilePath, seed :: Int}
            | Interpret {input :: FilePath}
            | Explore {input :: FilePath, stateful :: Int, cutoff :: Int}
            | Debug {input :: FilePath, cutoff :: Int}
            | Test 
  deriving (Show, Data, Typeable, Eq)

frontendMode :: Option
frontendMode = Frontend {input = def &= args} &= help _helpFE

middleendMode :: Option
middleendMode = Middleend {input = def &= args} &= help _helpFE

executeMode :: Option
executeMode = Execute {input = def &= args
                      ,seed = def &= help "seed for the scheduler"} &= help _helpExec

interpretMode :: Option
interpretMode = Interpret {input = def &= args} &= help _helpInter

debugMode :: Option
debugMode = Debug {input = def &= args
                  ,cutoff = def} &= help _helpDebug

exploreMode :: Option
exploreMode = Explore {input = def &= args
                      ,stateful = def &= help "stateful mode (0=False [default], 1=True)"
                      ,cutoff = def &= help "cutoff mode (0=False [default], 1=True)"} &= help _helpExplore

testMode :: Option
testMode = Test {} &= help _helpTest

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [frontendMode, middleendMode, executeMode, interpretMode, exploreMode, testMode, debugMode]
         &= help _help
         &= program _program
         &= summary _summary
         
-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options
          
runOption :: Option -> IO ()
runOption (Frontend f) = frontend f
runOption (Middleend f) = do
    prog <- extract f
    let (prog', fflow, flow, thcount) = frontEnd prog
        ind = snd $ convert prog' fflow flow thcount
    print ind
runOption (Execute f seed) = execute f seed
runOption (Interpret f) = interpreter f
runOption (Explore f stmode cutoffs) =  explore f (not $ toBool stmode) (toBool cutoffs)
runOption (Debug f cutoffs) = debug f False (toBool cutoffs)
runOption Test = test


frontend :: FilePath -> IO ()
frontend f = do
  prog <- extract f
  let prog' = frontEnd prog
  putStrLn "ORIGINAL PROGRAM"
  putStrLn "------------------------------"
  print prog
  putStrLn "TRANSFORMED PROGRAM"
  putStrLn "------------------------------"
  print prog'
    
interpreter :: FilePath -> IO ()
interpreter f = do
  prog <- extract f
  let (prog', fflow, flow, thcount) = frontEnd prog
      k = interpret $ convert prog' fflow flow thcount
  print prog'
  print k

execute :: FilePath -> Int -> IO ()
execute f dseed = do
  prog <- extract f
  seed <- randomIO :: IO Int
  print $ "Using seed = " ++ show (seed,dseed)
  let (prog', fflow, flow, thcount) = frontEnd prog
  print prog'
  let gen = if dseed == 0
            then mkStdGen seed
            else mkStdGen dseed
      log = exec gen thcount $ convert prog' fflow flow thcount
  putStrLn log

explore :: FilePath -> Bool -> Bool -> IO ()
explore f mode cutoffs = do
  --start <- getCurrentTime
  prog <- extract f
  let (prog', fflow, flow, thcount) = frontEnd prog
      (sys, indr) = convert prog' fflow flow thcount
      (size,nconf, ncutoff, sumsize) = runST (Exp.unfolder mode cutoffs sys indr >>= \s -> return (cntr s, maxConf s, cutoffCntr s, cumsize s)) --unfToDot)
  --putStrLn "Simple C generated by front-end:"
  --print prog'
  --stop <- getCurrentTime
  putStrLn $ "total number of events of the unfolding: " ++ show size
  putStrLn $ "total number of maximal configurations: " ++ show nconf
  putStrLn $ "total number of cutoffs: " ++ show ncutoff
  putStrLn $ "average size of U at maximal configurations: " ++ show (div sumsize (toInteger nconf))
  --putStrLn $ "execution time(s): " ++ (show $ diffUTCTime stop start)
  --writeFile (replaceExtension f ".dot") unfst

debug :: FilePath -> Bool -> Bool -> IO ()
debug f mode cutoffs = do 
    prog <- extract f
    let (prog', fflow, flow, thcount) = frontEnd prog
        (sys, indr) = convert prog' fflow flow thcount
        pes = runST (Exp.unfolder mode cutoffs sys indr >>=  \s -> showEvents $ evts s)
    putStrLn pes
    
test :: IO ()
test = do
    succCount <- runTestTT tests
    print succCount
    
{-
runPT :: FilePath -> IO ()
runPT file = do
  net <- parse file
  let ind = retrieveIndRel net
      unfSt = runST (convert net >>= \sys -> stateless sys ind >>= return . show)
  print unfSt 
-}
