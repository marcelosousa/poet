{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Util.CmdOpts 
-- Desc      :  Utilities for the command line options
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Util.CmdOpts where

import System.Console.CmdArgs
import System.FilePath.Posix

-- Command Line Options Strings
_program, _summary :: String
_program = "poet"
_summary =
  unlines ["POET - v0.2.1","Partial Order Exploration Tools"
          ,"is a set of exploration methods for concurrent C programs."
          ,"Copyright 2015-16 @ Marcelo Sousa"]
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

data Domain = Concrete | Interval
  deriving (Show, Data, Typeable, Eq, Enum)
                      
instance Default Domain where
  def = Concrete
  
data Option 
  = Frontend  {inp :: FilePath}
  | Execute   {inp :: FilePath, dom :: Domain, seed :: Int}
  | Interpret {inp :: FilePath, dom :: Domain}
  | Explore   {inp :: FilePath, dom :: Domain, stf :: Int, cut :: Int}
  | Prime     {inp :: FilePath, dom :: Domain, stf :: Int, cut :: Int}
  | Stid      {inp :: FilePath,                stf :: Int, cut :: Int}
  | Debug     {inp :: FilePath, dom :: Domain,              cut:: Int}
  | Test 
  deriving (Show, Data, Typeable, Eq)

-- | Mode options
frontend_mode, execute_mode, interpret_mode :: Option
frontend_mode =
  Frontend
  { inp = def &= args 
  } &= help _helpFE

execute_mode =
  Execute 
  { inp  = def
  , dom  = def
  , seed = def &= help "seed for the scheduler"
  } &= help _helpExec

interpret_mode =
  Interpret 
  { inp = def
  , dom = def &= args
  } &= help _helpInter

debug_mode = 
  Debug 
  { inp = def
  , dom = def 
  , cut = def
  } &= help _helpDebug

explore_mode =
  Explore
  { inp = def
  , dom = def
  , stf = def &= help "stf mode (0=False [default], 1=True)"
  , cut = def &= help "cut mode (0=False [default], 1=True)"
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
  cmdArgsMode $ modes [ frontend_mode, execute_mode, interpret_mode
                      , explore_mode, test_mode, debug_mode
                      , prime_mode, stid_mode ]
         &= help _help
         &= program _program
         &= summary _summary
