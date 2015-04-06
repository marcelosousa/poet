module Converter where

import Data.Maybe
import Control.Monad.ST.Safe
import Control.Monad
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H

import Language.SimpleC.AST
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.C.Syntax.AST (CBinaryOp(..),CUnaryOp(..))
import Model.GCS
import Frontend
import Frontend.Util

import Debug.Trace

-- read write data type
data RW = Read Var | Write Var
  deriving (Show,Eq,Ord)

type RWSet = [RW]

convert :: Program -> FirstFlow -> Flow -> Int -> ST s (System s, UIndep)
convert (Program (decls, defs)) pcs flow thCount = do
  -- @ get the initial local state: this will be the set of global variables 
  --   minus the pcs
  let ils = getInitialDecls decls
      pmd = (BS.pack "__poet_mutex_death", IntVal 1)
      pmt = (BS.pack "__poet_mutex_threads", Array $ map IntVal $ replicate (thCount+1) 1)
      ipcs = map (\(i,pc) -> (BS.pack ("pc."++i), IntVal pc)) pcs
  is <- toInitialState $ pmd:pmt:(ils++ipcs)
  (trs,annot) <- getTransitions defs >>= return . unzip 
  -- @ complete the initial state with the pcs: put the pc = 1 for main (later on put this on a user defined function)
  let vtrs = V.fromList trs
      uind = computeUIndep (map fst ils) annot
      sys = System vtrs is ils  
  return (sys, uind) 

computeUIndep :: [Var] -> [(TransitionID, RWSet)] -> UIndep
computeUIndep globals = undefined

getInitialDecls :: Decls -> LSigma
getInitialDecls = foldl (\a decl -> convertDecl decl ++ a) [] 
  where 
    convertDecl decl = case decl of
      FunctionDecl _ _ _ -> [] 
      GlobalDecl _ (Ident i) Nothing -> [(BS.pack i, IntVal 0)]
      GlobalDecl _ (Ident i) (Just (IntValue v)) -> [(BS.pack i, IntVal $ fromInteger v)]
      GlobalDecl _ (Index (Ident i) _) _ -> error "getInitialDecls: global array is not supported yet"
      _ -> error "getInitialState: not supported yet"

-- @ computeInitialState 
toInitialState :: LSigma -> ST s (ISigma s)
toInitialState lst = do
  ht <- H.new
  mapM_ (\(n,v) -> H.insert ht n v) lst
  return ht  

-- for each transition: 
-- type Transition s = (ProcessID, TransitionID, TransitionFn s)
-- process id is the name of the function
-- transition id is the position in the vector of transitions 
getTransitions :: Defs -> ST s [(Transition s, (TransitionID, RWSet))] 
getTransitions = undefined


