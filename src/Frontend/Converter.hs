module Converter where

import Data.Maybe
import Control.Monad.ST.Safe
import Control.Monad
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident

import Language.SimpleC.AST
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.C.Syntax.AST (CBinaryOp(..),CUnaryOp(..))
import Model.GCS

import Debug.Trace

-- the idea is to get the initial state so that we can 
-- know what are the global variables:
-- This is crucial to compute the independence relation
-- which has to be done at the same as we compute the 
-- transitions

parseFile :: FilePath -> IO CTranslUnit
parseFile f  =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] f
     case parse_result of
       Left parse_err -> do 
           parse_result <- parseCFilePre f
           case parse_result of
               Left _ -> error (show parse_err)
               Right ast -> return ast
       Right ast      -> return ast

pp :: FilePath -> IO ()
pp f = do ctu <- parseFile f
          print $ translate ctu

-- read write data type
data RW = Read Var | Write Var
  deriving (Show,Eq,Ord)

type RWSet = [RW]

convert :: Program -> ST s (System s, UIndep)
convert (Program (decls, defs)) = do
  -- @ get the initial local state: this will be the set of global variables 
  --   minus the pcs
  let ils = getInitialState decls
  is <- computeInitialState ils 
  (trs,annot) <- getTransitions defs >>= return . unzip 
  -- @ complete the initial state with the pcs: put the pc = 1 for main (later on put this on a user defined function)
  let vtrs = V.fromList trs
      uind = computeUIndep (map fst ils) annot
      sys = System vtrs is ils  
  return (sys, uind) 

computeUIndep :: [Var] -> [(TransitionID, RWSet)] -> UIndep
computeUIndep globals = undefined

getInitialState :: Decls -> LSigma
getInitialState = foldl (\a decl -> convertDecl decl ++ a) [(BS.pack "pcmain", 0)] 
  where 
    convertDecl decl = case decl of
      FunctionDecl _ _ _ -> [] 
      GlobalDecl _ i Nothing -> [(BS.pack i, 0)]
      GlobalDecl _ i (Just (IntValue v)) -> [(BS.pack i, fromInteger v)] 
      _ -> error "getInitialState: not supported yet"

-- @ computeInitialState 
computeInitialState :: LSigma -> ST s (ISigma s)
computeInitialState lst = do
  ht <- H.new
  mapM_ (\(n,v) -> H.insert ht n v) lst
  return ht  

-- for each transition: 
-- type Transition s = (ProcessID, TransitionID, TransitionFn s)
-- process id is the name of the function
-- transition id is the position in the vector of transitions 
getTransitions :: Defs -> ST s [(Transition s, (TransitionID, RWSet))] 
getTransitions = undefined


