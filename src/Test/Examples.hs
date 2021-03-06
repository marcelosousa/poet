module Test.Examples where

import Exploration.UNF.Unfolder
import Exploration.UNF.APIStateless
import Test.Examples.ExOne
import Test.Examples.ExTwo
import Test.Examples.ExThree
import Test.Examples.ExFour
import Test.Examples.ExFive
import Test.Examples.ExSix
import Test.Examples.ExSeven
import Test.Examples.ExEight
import Test.Examples.ExNine
import Test.Examples.ExTen
import qualified Test.Examples.ExEleven as Ex11

import Control.Monad.ST
import Test.HUnit

runTest1 mode = do
  let evs = runST (unfolder mode False sys1 ind11 >>= showEvents . evts)
  putStrLn evs

runTest2 mode = do
  let evs = runST (unfolder mode False sys1 ind12 >>= showEvents . evts)
  putStrLn evs

runTest3 mode = do
  let evs = runST (unfolder mode False sys2 ind2 >>= showEvents . evts)
  putStrLn evs

runTest4 mode = do
  let evs = runST (unfolder mode False sys3 ind3 >>= showEvents . evts)
  putStrLn evs

runTest5 mode = do
  let evs = runST (unfolder mode False sys4 ind4 >>= showEvents . evts)
  putStrLn evs

runTest6 mode = do
  let evs = runST (unfolder mode False sys5 ind5 >>= showEvents . evts)
  putStrLn evs

runTest7 mode = do
  let evs = runST (unfolder mode False sys6 ind6 >>= showEvents . evts)
  putStrLn evs
  
runTest8 mode = do
  let evs = runST (unfolder mode False sys7 ind7 >>= showEvents . evts)
  putStrLn evs

runTest9 mode = do
  let evs = runST (unfolder mode False sys8 ind8 >>= showEvents . evts)
  putStrLn evs

runTest10 mode = do
  let evs = runST (unfolder mode True sys9 ind9 >>= showEvents . evts)
  putStrLn evs

runTest11 mode = do
  let evs = runST (unfolder mode False sys10 ind10 >>= showEvents . evts)
  putStrLn evs

{-
-- Example 6 - very simple cyclic state space
t1_6, t2_6 :: Transition
t1_6 = ("p", "t1", t1_6')
t2_6 = ("q", "t2", t2_6')

t1_6', t2_6' :: Sigma -> Maybe Sigma
t1_6' s = Just $ M.insert "x" 1 s
t2_6' s = case M.lookup "x" s of
  Nothing -> error "cant happen"
  Just x -> Just $ M.insert "l" x s

s6 :: Sigma
s6 = M.singleton "x" 0

sys6 :: System
sys6 = ([t1_6, t2_6], s6)

ind6 :: UIndependence
ind6 = []

-- Example 7 - Fib from the SV COMP
t1_7, t2_7 :: Transition
t1_7 = ("p", "t1", t1_7')
t2_7 = ("q", "t2", t2_7')

num = 5

t1_7', t2_7' :: Sigma -> Maybe Sigma
t1_7' s = case M.lookup "k1" s of
  Nothing -> error "cant happen"
  Just k  -> 
    if k < num
    then 
      let i = fromMaybe (error "cant happen") $ M.lookup "i" s
          j = fromMaybe (error "cant happen") $ M.lookup "j" s
          j' = i + j
          check = i >= 145 || j' >= 145
      in if check
         then error "ASSERT FAIL"
         else Just $ M.insert "k1" (k+1) $ M.insert "j" j' s 
    else Nothing

t2_7' s = case M.lookup "k2" s of
  Nothing -> error "cant happen"
  Just k  -> 
    if k < num
    then 
      let i = fromMaybe (error "cant happen") $ M.lookup "i" s
          j = fromMaybe (error "cant happen") $ M.lookup "j" s
          i' = i + j
          check = j >= 145 || i' >= 145
      in if check
         then error "ASSERT FAIL"
         else Just $ M.insert "k2" (k+1) $ M.insert "i" i' s 
    else Nothing

s7 :: Sigma
s7 = M.insert "i" 1 $ M.insert "j" 1 $ M.insert "k1" 0 $ M.singleton "k2" 0

sys7 :: System
sys7 = ([t1_7, t2_7], s7)

ind7 :: UIndependence
ind7 = []
-}
