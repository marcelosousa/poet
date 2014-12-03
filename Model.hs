module Model where

import qualified Data.Map as M
import Data.Map hiding (foldr, filter, map)
import Data.List

type System = ([Transition], ISigma)
type ISigma = Sigma
type Sigma = Map Var Value
type Var = String
type Value = Int

--type Process = Map TransitionID Transition
type ProcessID = String

type TransitionID = String
type TransitionsID = [TransitionID]
type Transition = (ProcessID, TransitionID, TransitionFn)
type TransitionFn = Sigma -> Maybe Sigma

type UIndependence = [(TransitionID, TransitionID)]

getIState :: System -> ISigma
getIState = snd

dependent :: UIndependence -> TransitionID -> TransitionID -> Bool
dependent indep "bot" _ = True
dependent indep _ "bot" = True
dependent indep t t' = not $ (t,t') `elem` indep || (t',t) `elem` indep

enabledTransitions :: System -> Sigma -> TransitionsID
enabledTransitions sys@(trans,_) s = 
    let tr = filter (\(_,_,t) -> maybe False (const True) $ t s) trans
    in map snd3 tr

sortSigmas :: [Sigma] -> [Sigma]
sortSigmas sts = 
  let sts' = map filterSigma sts
  in sort sts'

filterSigma :: Sigma -> Sigma
filterSigma st = M.filter (==1) st
   
runSys :: System -> [Sigma] -> [Sigma]
runSys sys [] = []
runSys sys (x:xs) = 
  let ys = runSt sys x
      ys' = runSys sys $ ys ++ xs
  in nub $ (x:ys) ++ ys'

runSys' :: System -> [Sigma] -> [Sigma] -> [Sigma]
runSys' sys [] vs = vs
runSys' sys (x:xs) vs = 
  if x `elem` vs 
  then runSys' sys xs vs
  else 
    let ys = runSt sys x
        yys = nub $ ys ++ xs -- need to visit these
        ys' = runSys' sys yys (x:vs)
    in ys' 

runSt :: System -> Sigma -> [Sigma]
runSt (trs,_) st = 
  concatMap (\t -> runT t st) trs 

runT :: Transition -> Sigma -> [Sigma]
runT (_,_,t) s = case t s of
  Nothing -> []
  Just s' -> [s']

bot :: TransitionFn
bot s = Just s
    
getTransition :: System -> TransitionID -> TransitionFn
getTransition _ "bot" = bot
getTransition (trs,_) tr = 
    let [(_,_,t)] = filter ((tr ==) . snd3) trs
    in t
    
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- Abstract Interpretation
data Sign = GtZero | LtZero | Zero | Top | Bottom
  deriving (Show, Eq, Ord)

int2sign :: Int -> Sign
int2sign 0 = Zero
int2sign x 
  | x > 0 = GtZero
  | x < 0 = LtZero
 
-- alpha :: Lattice Concrete -> Lattice Abstract
-- example for sign analysis
alpha :: [Int] -> Sign
alpha [] = Bottom
alpha xs =   
  let (s:sxs) = map int2sign xs
  in alpha' s sxs 
  where
   alpha' s [] = s
   alpha' s (s':xs) = 
     if s == s'
     then alpha' s xs
     else Top 
  

