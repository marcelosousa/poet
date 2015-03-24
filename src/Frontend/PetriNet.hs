module Frontend.PetriNet where

import Control.Monad
import Control.Monad.ST.Safe
import Control.Monad.Trans

import qualified Model.GCS as GCS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V
import Data.List
import Debug.Trace

-- conceptual view of a Petri Net
--data Net = Net {
--  places :: Places
--, transitions :: Transitions
--, flow :: Flow
--, marking :: Place -> Int
--}

type Net = (Places, [Transitions])
type Places = [Place]
type Place = (GCS.Var, GCS.Value)
type Transitions = [Transition]
type Transition = (String, [GCS.Var], [GCS.Var])

fst3 :: (a, b, c) -> a 
fst3 (a,b,c) = a

-- | getSysInd - Given a file path to a petri net, parse and converts 
-- it into the model of computation of the net and computes the indep. rel.
getSysInd :: FilePath -> IO (ST s (GCS.System s), GCS.UIndep)
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
      trn = groupTr tr
  in (ps,trn) 
--  in if length trs == nt
--     then (ps,tr)
--     else error "parseNet"

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

getPlace :: Places -> String -> GCS.Var
getPlace ps s = 
  let i = read s :: Int
  in if length ps >= i
     then fst $ ps !! i
     else error "getPlace"     

-- Retrieve the independence relation
retrieveIndRel :: Net -> GCS.UIndep
retrieveIndRel (_, tr) =
  let matrixSize = length tr
  in V.generate matrixSize (\i -> 
       V.generate matrixSize (\j -> 
         check (tr!!i) (tr!!j)))   

check :: Transitions -> Transitions -> Bool
check t1s t2s = 
  let (t1,_,_) = head t1s
      (t2,_,_) = head t2s
      (pre,pos)   = foldr (\(_,p1,p2) (r1,r2) -> (nub $ p1 ++ r1, nub $ p2 ++ r2)) ([],[]) t1s
      (pre',pos') = foldr (\(_,p1,p2) (r1,r2) -> (nub $ p1 ++ r1, nub $ p2 ++ r2)) ([],[]) t2s
      b1 = pos `intersect` pre'
      b2 = pos' `intersect` pre
  in t1 /= t2 && null b1 && null b2    
-- previous check
-- check (t1,pre,pos) (t2,pre',pos') = 
--   let b1 = pos `intersect` pre'
--       b2 = pos' `intersect` pre
--   in t1 /= t2 && null b1 && null b2

-- 
groupTr :: Transitions -> [Transitions]
groupTr = groupBy (\(n,_,_) (m,_,_) -> dropSuffix n == dropSuffix m)  

dropSuffix :: String -> String
dropSuffix [] = []
dropSuffix ('_':'_':xs) = []
dropSuffix (x:xs) = x:dropSuffix xs 

-- Conversion section
convert :: Net -> ST s (GCS.System s)
convert net@(ps,tr) = do
  i <- H.fromList ps
  trs <- mapM toTransition $ zip tr [0..]
  return $ GCS.System (V.fromList trs) i ps
 
toTransition :: (Transitions, GCS.TransitionID) -> ST s (GCS.Transition s)
toTransition ([], tID) = error "toTransition"
toTransition (trs, tID) =  do
  let (n,_,_) = head trs
      fn = buildFn' $ zip trs [0..]
  return (BS8.pack $ dropSuffix n, tID, fn) 

buildFn' :: [(Transition,Int)] -> GCS.TransitionFn s
buildFn' trs = \s -> do 
  trs' <- mapM (\((na,pre,pos),n) -> mapM (\p -> H.lookup s p >>= return . checkLookup) pre >>= \pre' -> return (pre',n)) trs
  let ptrs = filter (\(pre',n) -> all (>0) pre') trs'
  case ptrs of
    [] -> return Nothing
    [(_,n)] -> do
      let ((na,pre,pos),_) = trs !! n
      return $ Just $ \s -> do
        foldM updatePre s pre
        foldM updatePos s pos
        return (s,map (\v -> (v,0)) pre ++ map (\v -> (v,1)) pos)
    _ -> error "buildFn': several transitions are enabled"

buildFn :: [GCS.Var] -> [GCS.Var] -> GCS.TransitionFn s
buildFn pre pos = \s -> do 
  pre' <- mapM (\p -> H.lookup s p >>= return . checkLookup) pre -- prem :: ST s [GCS.Value]
  if all (>0) pre'
  then do
    return $ Just $ \s -> do
      foldM updatePre s pre
      foldM updatePos s pos
      return (s,map (\v -> (v,1)) pos)
  else return Nothing 

getValue :: [GCS.Var] -> GCS.Sigma s -> ST s Bool
getValue pre s = do 
  prem <- mapM (\p -> H.lookup s p >>= return . checkLookup) pre
  return $ all (>0) prem
 
checkLookup :: Maybe GCS.Value -> GCS.Value
checkLookup Nothing  = error "checkLookup"
checkLookup (Just i) = i

updatePre :: GCS.Sigma s -> GCS.Var -> ST s (GCS.Sigma s)
updatePre s p = do
  H.insert s p 0
  return s  

updatePos :: GCS.Sigma s -> GCS.Var -> ST s (GCS.Sigma s)
updatePos s p = do 
  H.insert s p 1
  return s 
