{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------

module Main where

import System.Console.CmdArgs

import Control.Monad.ST

import Frontend (frontEnd)
import Language.SimpleC
import Converter
import Model.Interpreter

--import Unfolderful
--import Exploration.UNF.Unfolderless
--import Printer
--import Benchmark
--import Model.GCS
--import Frontend.PetriNet
--import qualified Data.Map as M
--import Control.Monad.ST.Safe
--import Test.HUnit
--import Test.Tests
--import Test.Examples

_program, _summary :: String
_summary = unlines ["POET - v0.1","Partial Order Exploration Tools is a set of exploration methods for concurrent C programs.","Copyright 2015 @ Marcelo Sousa"]
_program = "poet"
_help    = "The input files of poet are C files written in a restricted subset of the C language. For more info, check the documentation!"
_helpFE = unlines ["poet frontend receives a concurrent C program in a restricted subset of the language and performs a series of transformations to simplify the analysis"
                  ,"Example: poet frontend file.c"]

data Option = Frontend {input :: FilePath}
            | Middleend {input :: FilePath}
  deriving (Show, Data, Typeable, Eq)

frontendMode :: Option
frontendMode = Frontend {input = def &= args} &= help _helpFE

middleendMode :: Option
middleendMode = Middleend {input = def &= args} &= help _helpFE

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [frontendMode, middleendMode]
         &= help _help
         &= program _program
         &= summary _summary
         
-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options
          
runOption :: Option -> IO ()
runOption (Frontend f) = do
    prog <- extract f
    let prog' = frontEnd prog
    putStrLn "ORIGINAL PROGRAM"
    putStrLn "------------------------------"
    print prog
    putStrLn "TRANSFORMED PROGRAM"
    putStrLn "------------------------------"
    print prog'
runOption (Middleend f) = do
    prog <- extract f
    let (prog', fflow, flow, thcount) = frontEnd prog
        ind = runST (convert prog' fflow flow thcount >>= return . snd)
    print ind
    --convert :: Program -> FirstFlow -> Flow -> Int -> ST s (System s, UIndep)
    
exec :: FilePath -> IO ()
exec f = do
  prog <- extract f
  let (prog', fflow, flow, thcount) = frontEnd prog
      k = runST (convert prog' fflow flow thcount >>= interpreter . fst)    
  print k