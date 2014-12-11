{-#LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module Model where

import Control.Monad.ST.Safe
import Control.Monad
import Control.Monad.Trans.Maybe

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

import Debug.Trace

-- System is a vector of transitions and an initial state
-- We use vector because we just want to query. 
data System s = 
  System {
    transitions :: V.Vector (Transition s),
    initialState :: ISigma s
  }

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
type TransitionFn s = Sigma s -> ST s (Maybe (Sigma s -> ST s (Sigma s)))

-- This type class is important to define for each model of 
-- computation the enabled function that computes the transitions
-- that are enabled at a given state sigma.
-- The default enabled function is given below (enabledTransitions).
-- However, this function is very inefficient because it relies on 
-- actually computing the result for every transition.

-- | enabledTransitions 
enabledTransitions :: System s -> Sigma s -> ST s TransitionsID
enabledTransitions sys@System{..} s = do
  s' <- copy s
  tr <- V.filterM (\(_,_,t) -> t s' >>= return . maybe False (const True)) transitions  
  V.mapM (return . snd3) tr

-- An independence relation is an irreflexive and symmetric relation
-- Triangular Matrix to exploit symmetry of independence relation
-- The irreflexivity can be simply checked with equality which is O(1) 
type UIndep = V.Vector (V.Vector Bool)

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

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
bot s = return $ Just return    

-- GETTERS

-- | getTransition - 
getTransition :: System s -> TransitionID -> TransitionFn s
getTransition sys@System{..} trIdx
  | trIdx == botID = bot
  | otherwise = 
      case transitions V.!? trIdx of
        Nothing -> error $ "getTransition fail: " ++ show trIdx
        Just (_,_,tr) -> tr

-- | Runs the system in a dfs fashion
runSystem :: System s -> ST s [Sigma s]
runSystem sys@System{..} = runSys 1 sys [initialState] []

runSys :: Int -> System s -> [Sigma s] -> [Sigma s] -> ST s [Sigma s]
runSys c sys [] sts = return sts
runSys c sys (s:rest) sts = trace ("runSys: " ++ show c) $ do
  contained <- isElem s sts 
  if contained 
  then runSys c sys rest sts 
  else do 
    ys <- runTrs sys s
    let stack = ys ++ rest 
 -- stack' <- foldM (\a v -> add v a) rest ys -- I can fuse these two lines
--  stack  <- filterM (\s -> isElem s nsts >>= return . not) stack'
    runSys (c+1) sys stack (s:sts)

runTrs :: System s -> Sigma s -> ST s [Sigma s]
runTrs sys s = do
  trs <- enabledTransitions sys s
  V.foldM runTrs' [] trs
  where 
    runTrs' acc tr = do
      s' <- copy s                     -- copy the state
      v <- (getTransition sys tr) s'     -- gets the transition and applies it
      ns <- fromJust v $ s'            -- now I actually apply it
      return $ ns:acc

-- This needs to be more efficient
copy :: Sigma s -> ST s (Sigma s)
copy s = do 
  kv <- H.toList s
  H.fromList kv 

-- Add a state to a list of states if that state is not already in the list
add :: Sigma s -> [Sigma s] -> ST s [Sigma s]
add s sts = do
    return $ s:sts
--  isEl <- isElem s sts
--  if isEl
--  then return sts
--  else return $ s:sts

-- This is the bottleneck
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
  return $ isEqual' l1 l2  

isEqual' :: [(Var,Value)] -> [(Var,Value)] -> Bool
isEqual' [] [] = True
isEqual' ((x,v):xs) ((y,t):ys) = x == y && v == t && isEqual' xs ys

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
