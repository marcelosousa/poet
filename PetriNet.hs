module PetriNet where

import qualified Model as ML
import qualified Data.Map as M
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
type Place = (Id, Int)
type Transitions = [Transition]
type Transition = (Id, [Id], [Id])
type Id = String 

fst3 :: (a, b, c) -> a 
fst3 (a,b,c) = a

getSysInd :: FilePath -> IO (ML.System, ML.UIndependence)
getSysInd file = do
  net <- parse file
  let sys = convert net
      ind = nub $ retrieveIndRel net
  return (sys, ind)

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
      in (name, m)
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
         else error "parseTransition: length pos is not correct"
    _ -> error $ "parseTransition " ++ s

getPlace :: Places -> String -> Id
getPlace ps s = 
  let i = read s :: Int
  in if length ps >= i
     then fst $ ps !! i
     else error "getPlace"     

convert :: Net -> ML.System
convert (ps,tr) = 
  let i = M.fromList ps
      t = map toTransition tr
  in (t, i)

retrieveIndRel :: Net -> ML.UIndependence
retrieveIndRel (_, tr) = 
  let l = [ (t,t') | t <- tr, t' <- tr, check t t' ]
  in map (\(a,b) -> (fst3 a, fst3 b)) l

check :: Transition -> Transition -> Bool
check (t1,pre,pos) (t2,pre',pos') = 
  let b1 = pos `intersect` pre'
      b2 = pos' `intersect` pre
  in t1 /= t2 && null b1 && null b2

toTransition :: Transition -> ML.Transition
toTransition (n,pre,pos) = 
  let fn = buildFn pre pos
  in (n,n,fn) 

buildFn :: [Id] -> [Id] -> ML.TransitionFn
buildFn pre pos = \s -> 
  let pre' = map (\p -> lookup' p s) pre
      spre = foldr updatePre s pre
      spos = foldr updatePos spre pos
  in if all (>0) pre'
     then Just spos
     else Nothing

lookup' :: Id -> ML.Sigma -> Int
lookup' s m = case M.lookup s m of
  Nothing -> error "lookup'"
  Just i  -> i

updatePre :: Id -> ML.Sigma -> ML.Sigma
updatePre p s = M.alter update' p s
  where update' Nothing  = error "updatePre"
        update' (Just i) = Just $ i-1

updatePos :: Id -> ML.Sigma -> ML.Sigma
updatePos p s = M.alter update' p s
  where update' Nothing  = Just 1
        update' (Just i) = Just $ i+1
