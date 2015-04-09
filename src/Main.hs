{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------

module Main where

import Control.Monad.ST

import System.Console.CmdArgs
import System.FilePath.Posix
import System.Random

import Language.SimpleC
import Frontend (frontEnd)
import Converter
import Model.Interpreter
import Exploration.UNF.Unfolderless (stateless)
import Exploration.UNF.APIStateless hiding (execute)
import Util.Printer

import Test.HUnit (runTestTT)
import Test.Tests

--import Unfolderful
--import Printer
--import Benchmark
--import Model.GCS
--import Frontend.PetriNet
--import qualified Data.Map as M
--import Control.Monad.ST.Safe


_program, _summary :: String
_summary = unlines ["POET - v0.1","Partial Order Exploration Tools is a set of exploration methods for concurrent C programs.","Copyright 2015 @ Marcelo Sousa"]
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
_helpTest = unlines ["poet test runs the explore mode over the set of examples in Test/Examples"]
                   
data Option = Frontend {input :: FilePath}
            | Middleend {input :: FilePath}
            | Execute {input :: FilePath, seed :: Int}
            | Interpret {input :: FilePath}
            | Explore {input :: FilePath}
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

exploreMode :: Option
exploreMode = Explore {input = def &= args} &= help _helpExplore

testMode :: Option
testMode = Test {} &= help _helpTest

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [frontendMode, middleendMode, executeMode, interpretMode, exploreMode, testMode]
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
        ind = runST (convert prog' fflow flow thcount >>= return . snd)
    print ind
runOption (Execute f seed) = execute f seed
runOption (Interpret f) = interpreter f
runOption (Explore f) = explore f
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
      k = runST (convert prog' fflow flow thcount >>= interpret)    
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
      log = runST (convert prog' fflow flow thcount >>= exec gen thcount)  
  putStrLn log

explore :: FilePath -> IO ()
explore f = do
  prog <- extract f
  let (prog', fflow, flow, thcount) = frontEnd prog
      (size,unfst) = runST (convert prog' fflow flow thcount >>= uncurry (stateless False) >>= unfToDot)
  putStrLn "Simple C generated by front-end:"
  size `seq` print prog'
  putStrLn $ "total number of events of the unfolding: " ++ show size
  writeFile (replaceExtension f ".dot") unfst

test :: IO ()
test = do
    succCount <- runTestTT (tests False)
    print succCount
    
{-
runPT :: FilePath -> IO ()
runPT file = do
  net <- parse file
  let ind = retrieveIndRel net
      unfSt = runST (convert net >>= \sys -> stateless sys ind >>= return . show)
  print unfSt 
-}
