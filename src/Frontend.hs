module Frontend where

import Language.SimpleC.AST
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.C hiding (Ident)
import Language.C.System.GCC  -- preprocessor used

import Data.List
import Debug.Trace

import Frontend.Util
import Frontend.PassOne
import Frontend.PassTwo
import Frontend.PassThree
import Frontend.PassFourFive
import Frontend.PassSix
import Frontend.PassSeven

-- TODO: pass 8: transform the program into a graph (data type to be defined)

-- frontEnd: 
--  Transforms a C file into a restricted subset of C
--  for easy manipulation and analysis.
frontEnd :: Program -> Program
frontEnd prog = 
    let globals = getGlobalsDecls prog
        prog1 = passOne prog
        pass2res = passTwo globals prog1 -- :: Int
        prog3 = passThree globals prog1
        prog45 = passFourFive prog3
        prog6 = passSix prog45
        pass7res = pass2res `seq` passSeven prog6 -- :: Bool
    in if pass7res
       then prog6
       else error "frontEnd fatal: something went wrong! Please contact developers."

