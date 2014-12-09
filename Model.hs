module Model where

import Control.Monad.ST.Safe
import Control.Monad

-- Data Structures
import qualified Data.Map as M
import Data.Map hiding (foldr, filter, map, (\\))
import Data.List
import qualified Data.Judy as J
-- Perhaps I want to use Unboxed Vectors?
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import qualified Data.Word as W
import qualified Data.Set as S
import Data.Maybe 

-- System is a vector of transitions and an initial state
-- We use vector because we just want to query. 
type System s = (V.Vector (Transition s), ISigma s)
type ISigma s = Sigma s
-- A state is an Hash Table
type HashTable s k v = C.HashTable s k v
--  Later we can try Judy Arrays or Mutable Vectors
type Sigma s  = HashTable s Var Value -- We want to add the enabled transitions for efficiency.
type Var      = BS.ByteString
type Value    = Int

--type Process = Map TransitionID Transition
type ProcessID = BS.ByteString
type TransitionID = Int 
type TransitionsID = V.Vector TransitionID
type Transition s = (ProcessID, TransitionID, TransitionFn s)
type TransitionFn s = Sigma s -> Maybe (Sigma s)

-- This type class is important to define for each model of 
-- computation the enabled function that computes the transitions
-- that are enabled at a given state sigma.
-- The default enabled function is given below (enabledTransitions).
-- However, this function is very inefficient because it relies on 
-- actually computing the result for every transition.
class Executable s where
  enabled :: System s -> Sigma s -> TransitionsID

-- | hard core enabledTransitions - very inefficient! 
enabledTransitions :: System s -> Sigma s -> TransitionsID
enabledTransitions sys@(trans,_) s = 
    let tr = V.filter (\(_,_,t) -> maybe False (const True) $ t s) trans
    in V.map snd3 tr

-- An independence relation is an irreflexive and symmetric relation
-- Triangular Matrix to exploit symmetry of independence relation
-- The irreflexivity can be simply checked with equality which is O(1) 
type UIndep = V.Vector (V.Vector Bool)

-- END OF TYPES 
-- It is many times the case that we run mutually exclusive functions
-- like what i have below (min, max). Program consolidation could 
-- optimisize this code.
-- | isIndependent -- check if two transitions are uncond. indep.
isIndependent, isDependent :: UIndep -> TransitionID -> TransitionID -> Bool
isIndependent uindep t1 t2  
  | (t1 == botID) || (t2 == botID) || (t1 == t2) = False
  | otherwise = 
      let t  = min t1 t2
          t' = max t1 t2
      in uindep V.! t V.! t'
 
-- | isDependent - checks if two transitions are dependent
isDependent uindep t1 t2 = not $ isIndependent uindep t1 t2 

-- | botID 0 is the transition id for bottom 
botID :: TransitionID
botID = -1

-- | bottom transition is simply: return . id
bot :: TransitionFn s
bot s = Just s    

-- GETTERS

-- | getInitialState - get the initial state of a system
getInitialState :: System s -> ISigma s
getInitialState = snd

-- | getTransition - 
getTransition :: System s -> TransitionID -> TransitionFn s
getTransition (trs,_) trIdx
  | trIdx == botID = bot
  | otherwise = 
      case trs V.!? trIdx of
        Nothing -> error $ "getTransition fail: " ++ show trIdx
        Just (_,_,tr) -> tr

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- | Runs the system in a dfs fashion
runSystem :: Executable s => System s -> ST s [Sigma s]
runSystem sys@(_,i) = runSys sys [i] []

runSys :: Executable s => System s -> [Sigma s] -> [Sigma s] -> ST s [Sigma s]
runSys sys []     sts = return sts
runSys sys (s:rest) sts = do 
  let ys = runTrs sys s
      nsts = s:sts
  stack' <- foldM (\a v -> add v a) rest ys -- I can fuse these two lines
  stack  <- filterM (\s -> isElem s nsts >>= return . not) stack'
  runSys sys stack nsts

runTrs :: Executable s => System s -> Sigma s -> [Sigma s]
runTrs sys s = 
  let trs = V.toList $ enabled sys s 
  in map (\tr -> fromJust $ getTransition sys tr s) trs
 
-- Add a state to a list of states if that state is not already in the list
add :: Sigma s -> [Sigma s] -> ST s [Sigma s]
add s sts = do
  isEl <- isElem s sts
  if isEl
  then return sts
  else return $ s:sts

isElem :: Sigma s -> [Sigma s] -> ST s Bool
isElem s [] = return False
isElem s (x:xs) = do
  isEq <- isEqual s x
  if isEq 
  then return True
  else isElem s xs

-- State equality: because I'm using a hashtable I need to stay within the ST monad
isEqual :: Sigma s -> Sigma s -> ST s Bool
isEqual s1 s2 = do
  l1 <- H.toList s1
  l2 <- H.toList s2
  return $ l1 == l2  

{-
sortSigmas :: [Sigma s] -> [Sigma s]
sortSigmas sts = 
  let sts' = map filterSigma sts
  in sort sts'

filterSigma :: Sigma s -> Sigma s
filterSigma st = M.filter (==1) st
-}
{-
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
  

-}
