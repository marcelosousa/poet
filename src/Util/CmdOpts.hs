{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Util.CmdOpts 
-- Desc      :  Utilities for the command line options
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Util.CmdOpts where

import System.Console.CmdArgs
import System.FilePath.Posix
import Util.Generic

-- Command Line Options Strings
_program, _summary :: String
_program = "poet"
_summary =
  unlines ["POET - v0.2.2","Partial Order Exploration Tools"
          ,"Exploration methods for concurrent C programs based on event structures"
          ,"Copyright 2015-17 @ Marcelo Sousa"]
_help    = "The input files of poet are C files written in a restricted "
        ++ "subset of the C language."
        ++ "For more info, check the documentation!"
_helpFE =
  unlines ["poet frontend receives a concurrent C program in a restricted "
        ++ "subset of the language and performs a series of transformations "
        ++ "to simplify the analysis."
         , "Example: poet frontend file.c"]
_helpInter =
  unlines ["poet interpret receives a concurrent C program in a restricted "
        ++ "subset of the language and runs the interpreter on the "
        ++ "corresponding model of computation."
         , "Example: poet interpret -i=file.c"]
_helpExec =
  unlines ["poet execute receives a concurrent C program in a restricted "
        ++ "subset of the language and executes one run of the corresponding "
        ++ "model of computation."
         , "Example: poet execute -i=file.c -s=int (optional)"]                 
_helpExplore =
  unlines ["poet explore receives a concurrent C program in a restricted "
        ++ "subset of the language and explores the state space of the "
        ++ "corresponding model of computation using a partial order "
        ++ "representation."
         , "Example: poet explore -i=file.c"]
_helpStid =
  unlines ["poet stid receives a concurrent C program as an LLVM .ll file "
        ++ "and explores the state space of this program using the STID FE."
         , "Example: poet stid -i=file.c"]
_helpDebug = "poet debug receives a concurrent C program in a restricted "
          ++ " subset of the language and explores the state space and "
          ++ "explores the unfolding. At the end, it prints the LPES."
_helpTest = "poet test runs the explore mode over the set of examples in "
         ++ "Test/Examples."
_helpAi = "poet ai -i=file.c runs the abstract interpreter for intervals"
                      
instance Default Analysis where
  def = Concrete
  
data Option 
  = Frontend  {inp :: FilePath}
  | Interpret {inp :: FilePath, dom :: Analysis, mode :: Int, seed :: Int, stf :: Int, cut :: Int, wid :: Int}
  | Explore   {inp :: FilePath, dom :: Analysis, stf :: Int, cut :: Int, wid :: Int}
  | Prime     {inp :: FilePath, dom :: Analysis, stf :: Int, cut :: Int}
  | Stid      {inp :: FilePath,                stf :: Int, cut :: Int}
  | Test 
  deriving (Show, Data, Typeable, Eq)

-- | Mode options
frontend_mode, interpret_mode :: Option
frontend_mode =
  Frontend
  { inp = def &= args 
  } &= help _helpFE

interpret_mode =
  Interpret 
  { inp = def
  , dom = def
  , mode = def &= help "mode of the interpreter (0=Executes [default], 1=Step By Step)"
  , seed = def &= help "seed for the scheduler"
  , stf  = def &= help "stateful mode (0=False [default], 1=True)"
  , cut  = def &= help "cut      mode (0=False [default], 1=True)"
  , wid  = 10  &= help "widening"
  } &= help _helpInter

explore_mode =
  Explore
  { inp = def
  , dom = def
  , stf = def &= help "stf mode (0=False [default], 1=True)"
  , cut = def &= help "cut mode (0=False [default], 1=True)"
  , wid = 10  &= help "widening"
  } &= help _helpExplore

prime_mode =
  Prime 
  { inp = def
  , dom = def
  , stf = def &= help "stf mode (0=False [default], 1=True)"
  , cut = def &= help "cut mode (0=False [default], 1=True)"
  } &= help _helpExplore

stid_mode =
  Stid 
  { inp = def
  , stf = def &= help "stf mode (0=False [default], 1=True)"
  , cut = def &= help "cut mode (0=False [default], 1=True)"
  } &= help _helpStid


test_mode = Test {} &= help _helpTest

prog_modes :: Mode (CmdArgs Option)
prog_modes = 
  cmdArgsMode $ modes [ frontend_mode, interpret_mode
                      , explore_mode, test_mode
                      , prime_mode, stid_mode ]
         &= help _help
         &= program _program
         &= summary _summary
