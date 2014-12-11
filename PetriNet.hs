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
type Transition = (ML.Var, [ML.Var], [ML.Var])

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
         then (BS8.pack name, pre, pos)
         else error "parseTransition: length pos is not correct"
    _ -> error $ "parseTransition " ++ s

getPlace :: Places -> String -> ML.Var
getPlace ps s = 
  let i = read s :: Int
  in if length ps >= i
     then fst $ ps !! i
     else error "getPlace"     

-- Retrieve the independence relation
-- This has to change.
retrieveIndRel :: Net -> ML.UIndep
retrieveIndRel = undefined
{-
retrieveIndRel (_, tr) = 
  let l = [ (t,t') | t <- tr, t' <- tr, check t t' ]
  in map (\(a,b) -> (fst3 a, fst3 b)) l

check :: Transition -> Transition -> Bool
check (t1,pre,pos) (t2,pre',pos') = 
  let b1 = pos `intersect` pre'
      b2 = pos' `intersect` pre
  in t1 /= t2 && null b1 && null b2
-}
-- Conversion section
convert :: Net -> ST s (ML.System s)
convert net@(ps,tr) = do
  i <- H.fromList ps 
  trs <- mapM toTransition $ zip tr [0..]
  return $ ML.System (V.fromList trs) i

toTransition :: (Transition, ML.TransitionID) -> ST s (ML.Transition s)
toTransition ((n,pre,pos),tID) =  do
  let fn = buildFn pre pos
  return (n,tID,fn) 

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
