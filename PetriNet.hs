module PetriNet where

import Control.Monad
import Control.Monad.ST.Safe
import Control.Monad.Trans

import qualified Model as ML
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V
import Data.List

-- conceptual view of a Petri Net
--data Net = Net {
--  places :: Places
--, transitions :: Transitions
--, flow :: Flow
--, marking :: Place -> Int
--}

type Net = (Places, Transitions)
type Places = [Place]
type Place = (ML.Var, ML.Value)
type Transitions = [Transition]
type Transition = (String, [ML.Var], [ML.Var])

fst3 :: (a, b, c) -> a 
fst3 (a,b,c) = a

-- | getSysInd - Given a file path to a petri net, parse and converts 
-- it into the model of computation of the net and computes the indep. rel.
getSysInd :: FilePath -> IO (ST s (ML.System s), ML.UIndep)
getSysInd file = do
  net <- parse file
  sys <- return $ convert net
  let ind = retrieveIndRel net -- nub
  return (sys, ind)

-- | These functions take care of the parsing of the net
parse :: FilePath -> IO Net 
parse file = do
  f <- readFile file
  let linesOf = lines f
      net = parseNet linesOf
  return net

parseNet :: [String] -> Net
parseNet ("PT1":sp:st:rest) = 
  let np = read sp :: Int
      nt = read st :: Int
      (pls, trs) = splitAt np rest 
      ps = map parsePlace pls
      tr = map (parseTransition ps) trs
  in if length trs == nt
     then (ps,tr)
     else error "parseNet"

parsePlace :: String -> Place
parsePlace s = 
  case words s of
    [name,sm] -> 
      let m = read sm :: Int
      in (BS8.pack name, m)
    _ -> error $ "parsePlace " ++ s

parseTransition :: Places -> String -> Transition 
parseTransition p s =
  case words s of
    (name:snpre:snpos:rest) ->
      let npre = read snpre :: Int
          npos = read snpos :: Int
          rest' = map (getPlace p) rest 
          (pre,pos) = splitAt npre rest'
      in if length pos == npos
         then (name, pre, pos)
         else error $ "parseTransition: length pos is not correct: " ++ show (pos,npos)
    _ -> error $ "parseTransition " ++ s

getPlace :: Places -> String -> ML.Var
getPlace ps s = 
  let i = read s :: Int
  in if length ps >= i
     then fst $ ps !! i
     else error "getPlace"     

-- Retrieve the independence relation
retrieveIndRel :: Net -> ML.UIndep
retrieveIndRel (_, tr) =
  let matrixSize = length tr
  in V.generate matrixSize (\i -> 
       V.generate matrixSize (\j -> 
         check (tr!!i) (tr!!j)))   

check :: Transition -> Transition -> Bool
check (t1,pre,pos) (t2,pre',pos') = 
  let b1 = pos `intersect` pre'
      b2 = pos' `intersect` pre
  in t1 /= t2 && null b1 && null b2

-- 
groupTr :: Transitions -> [Transitions]
groupTr = groupBy (\(n,_,_) (m,_,_) -> dropSuffix n == dropSuffix m)  

dropSuffix :: String -> String
dropSuffix [] = []
dropSuffix ('_':'_':xs) = []
dropSuffix (x:xs) = x:dropSuffix xs 

-- Conversion section
convert :: Net -> ST s (ML.System s)
convert net@(ps,tr) = do
  i <- H.fromList ps
  let trn = groupTr tr 
  trs <- mapM toTransition $ zip trn [0..]
  return $ ML.System (V.fromList trs) i

toTransition :: (Transitions, ML.TransitionID) -> ST s (ML.Transition s)
toTransition ([], tID) = error "toTransition"
toTransition (trs, tID) =  do
  let (n,_,_) = head trs
      fn = buildFn' $ zip trs [0..]
  return (BS8.pack $ dropSuffix n, tID, fn) 

buildFn' :: [(Transition,Int)] -> ML.TransitionFn s
buildFn' trs = \s -> do 
  trs' <- mapM (\((na,pre,pos),n) -> mapM (\p -> H.lookup s p >>= return . checkLookup) pre >>= \pre' -> return (pre',n)) trs
  let ptrs = filter (\(pre',n) -> all (>0) pre') trs'
  case ptrs of
    [] -> return Nothing
    [(_,n)] -> do
      let ((_,pre,pos),_) = trs !! n
      return $ Just $ \s -> do
        foldM updatePre s pre
        foldM updatePos s pos
    _ -> error "buildFn': several transitions are enabled"

buildFn :: [ML.Var] -> [ML.Var] -> ML.TransitionFn s
buildFn pre pos = \s -> do 
  pre' <- mapM (\p -> H.lookup s p >>= return . checkLookup) pre -- prem :: ST s [ML.Value]
  if all (>0) pre'
  then do
    return $ Just $ \s -> do
      foldM updatePre s pre
      foldM updatePos s pos
  else return Nothing 

getValue :: [ML.Var] -> ML.Sigma s -> ST s Bool
getValue pre s = do 
  prem <- mapM (\p -> H.lookup s p >>= return . checkLookup) pre
  return $ all (>0) prem
 
checkLookup :: Maybe ML.Value -> ML.Value
checkLookup Nothing  = error "checkLookup"
checkLookup (Just i) = i

updatePre :: ML.Sigma s -> ML.Var -> ST s (ML.Sigma s)
updatePre s p = do
  H.insert s p 0
  return s  

updatePos :: ML.Sigma s -> ML.Var -> ST s (ML.Sigma s)
updatePos s p = do 
  H.insert s p 1
  return s 
