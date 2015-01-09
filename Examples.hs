module Examples where

import Model 

import Control.Monad.ST.Safe

import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H

import Data.Maybe

-- Example 1 - Two writes of different variables
-- Sigma s -> ST s (Maybe (Sigma s -> ST s (Sigma s)))
t1' :: TransitionFn s 
t1' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") 2
      H.insert s (BS.pack "x") 1
      return s 
    _ -> return Nothing

t1 :: Transition s
t1 = (BS.pack "p", 0, t1')

t2' :: TransitionFn s 
t2' s = do
  v <- safeLookup "t2" s (BS.pack "pcq")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") 2
      H.insert s (BS.pack "y") 1
      return s 
    _ -> return Nothing

t2 :: Transition s
t2 = (BS.pack "q", 1, t2')

s1 :: ST s (Sigma s)
s1 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") 1 
  H.insert ht (BS.pack "pcq") 1 
  H.insert ht (BS.pack "x") 0 
  H.insert ht (BS.pack "y") 0 
  return ht

sys1 :: ST s (System s)
sys1 = do 
  is <- s1
  return $ System (V.fromList [t1,t2]) is

ind11,ind12 :: UIndep
ind11 = V.generate 2 (\i -> V.generate 2 (\j -> False)) 
ind12 = V.generate 2 (\i -> V.generate 2 (\j -> if i /= j then True else False)) 

-- Example 2 - 1 write, 2 reads
s2 :: ST s (Sigma s)
s2 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") 1 
  H.insert ht (BS.pack "pcq") 1 
  H.insert ht (BS.pack "pcr") 1 
  H.insert ht (BS.pack "x") 0 
  H.insert ht (BS.pack "l1") 0 
  H.insert ht (BS.pack "l2") 0 
  return ht

t1_2, t2_2, t3_2 :: Transition s
t1_2 = (BS.pack "p",0,t1_2')
t2_2 = (BS.pack "q",1,t2_2')
t3_2 = (BS.pack "r",2,t3_2')

t1_2', t2_2', t3_2' :: TransitionFn s 
t1_2' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") 2
      H.insert s (BS.pack "x") 1
      return s 
    _ -> return Nothing
t2_2' s = do
  v <- safeLookup "t2" s (BS.pack "pcq")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") 2
      x <- safeLookup "t2" s (BS.pack "x")
      H.insert s (BS.pack "l1") x
      return s 
    _ -> return Nothing
t3_2' s = do
  v <- safeLookup "t3" s (BS.pack "pcr")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcr") 2
      x <- safeLookup "t3" s (BS.pack "x")
      H.insert s (BS.pack "l2") x
      return s 
    _ -> return Nothing

sys2 :: ST s (System s)
sys2 = do 
  is <- s2
  return $ System (V.fromList [t1_2,t2_2,t3_2]) is

ind2 :: UIndep
ind2 = V.generate 3 (\i -> V.generate 3 (\j -> check2 i j)) 
--ind2 = V.generate 3 (\i -> V.generate 3 (\j -> False)) 

check2 :: Int -> Int -> Bool
check2 1 2 = True
check2 2 1 = True
check2 _ _ = False

-- Example 3 - paper
s3 :: ST s (Sigma s)
s3 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") 1 
  H.insert ht (BS.pack "pcq") 1 
  H.insert ht (BS.pack "pcr") 1 
  H.insert ht (BS.pack "pcs") 1 
  H.insert ht (BS.pack "x") 0 
  H.insert ht (BS.pack "y") 0 
  H.insert ht (BS.pack "z") 0 
  return ht

t1_3, t2_3, t31_3, t32_3, t41_3, t42_3 :: Transition s
t1_3 = (BS.pack "p",0,t1_3')
t2_3 = (BS.pack "q",1,t2_3')
t31_3 = (BS.pack "r",2,t31_3')
t32_3 = (BS.pack "r",3,t32_3')
t41_3 = (BS.pack "s",4,t41_3')
t42_3 = (BS.pack "s",5,t42_3')

t1_3', t2_3', t31_3', t32_3', t41_3', t42_3' :: TransitionFn s 
t1_3' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") 2
      H.insert s (BS.pack "x") 1
      return s 
    _ -> return Nothing
t2_3' s = do
  v <- safeLookup "t2" s (BS.pack "pcq")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") 2
      H.insert s (BS.pack "y") 1
      return s 
    _ -> return Nothing
t31_3' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    1 -> return $ Just $ \s -> do
      y <- safeLookup "t31" s (BS.pack "y")
      case y of 
        0 -> H.insert s (BS.pack "pcr") 2
        _ -> H.insert s (BS.pack "pcr") 3
      return s 
    _ -> return Nothing
t32_3' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    2 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcr") 3
      H.insert s (BS.pack "z") 1
      return s 
    _ -> return Nothing
t41_3' s = do
  v <- safeLookup "t41" s (BS.pack "pcs")
  case v of
    1 -> return $ Just $ \s -> do
      y <- safeLookup "t41" s (BS.pack "z")
      case y of 
        1 -> H.insert s (BS.pack "pcs") 2
        _ -> H.insert s (BS.pack "pcs") 3
      return s 
    _ -> return Nothing
t42_3' s = do
  v <- safeLookup "t42" s (BS.pack "pcs")
  case v of
    2 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcs") 3
      H.insert s (BS.pack "x") 2
      return s 
    _ -> return Nothing

sys3 :: ST s (System s)
sys3 = do 
  is <- s3
  return $ System (V.fromList [t1_3,t2_3,t31_3,t32_3,t41_3,t42_3]) is

ind3 :: UIndep
ind3 = V.generate 6 (\i -> V.generate 6 (\j -> check3 i j)) 

-- [("t1","t2"),("t1","t31"),("t1","t32"),("t1","t41"),("t2","t32"),("t2","t41"),("t2","t42"),("t31","t41"),("t31","t42"),("t32","t42")]
check3 :: Int -> Int -> Bool
check3 0 1 = True
check3 1 0 = True
check3 0 2 = True
check3 2 0 = True
check3 0 3 = True
check3 3 0 = True
check3 0 4 = True
check3 4 0 = True
check3 1 3 = True
check3 3 1 = True
check3 1 4 = True
check3 4 1 = True
check3 1 5 = True
check3 5 1 = True
check3 2 4 = True
check3 4 2 = True
check3 2 5 = True
check3 5 2 = True
check3 3 5 = True
check3 5 3 = True
check3 _ _ = False

-- Example 4
s4 :: ST s (Sigma s)
s4 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") 1 
  H.insert ht (BS.pack "pcq") 1 
  return ht

t11_4, t12_4, t21_4, t22_4 :: Transition s
t11_4 = (BS.pack "p",0,t11_4')
t12_4 = (BS.pack "p",1,t12_4')
t21_4 = (BS.pack "q",2,t21_4')
t22_4 = (BS.pack "q",3,t22_4')

t11_4', t12_4', t21_4', t22_4' :: TransitionFn s 
t11_4' s = do
  v <- safeLookup "t11" s (BS.pack "pcp")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") 2
      H.insert s (BS.pack "y") 1
      return s 
    _ -> return Nothing
t12_4' s = do 
  v <- safeLookup "t12" s (BS.pack "pcp")
  case v of
    2 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") 3
      H.insert s (BS.pack "x") 1
      return s 
    _ -> return Nothing
t21_4' s = do 
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") 2
      H.insert s (BS.pack "z") 1
      return s 
    _ -> return Nothing
t22_4' s = do 
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
    2 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") 3
      H.insert s (BS.pack "x") 2
      return s 
    _ -> return Nothing

sys4 :: ST s (System s)
sys4 = do 
  is <- s4
  return $ System (V.fromList [t11_4,t12_4,t21_4,t22_4]) is

ind4 :: UIndep
ind4 = V.generate 4 (\i -> V.generate 4 (\j -> check4 i j)) 

-- ind4 = [("t1","t3"),("t1","t4"),("t2","t3")]
check4 :: Int -> Int -> Bool
check4 0 2 = True
check4 2 0 = True
check4 0 3 = True
check4 3 0 = True
check4 1 2 = True
check4 2 1 = True
check4 _ _ = False

-- Example 5 - Cesar's example
t1_5',t21_5',t22_5',t31_5',t32_5' :: TransitionFn s
t1_5' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    1 -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") 2
      H.insert s (BS.pack "x") 1
      return s 
    _ -> return Nothing
t21_5' s = do
  v <- safeLookup "t21" s (BS.pack "pcq")
  case v of
    1 -> return $ Just $ \s -> do
      lock <- safeLookup "t21" s (BS.pack "lock")
      case lock of
        0 -> do
          H.insert s (BS.pack "pcq") 2
          H.insert s (BS.pack "lock") 1
          return s 
        1 -> do 
          H.insert s (BS.pack "pcq") 3
          return s 
    _ -> return Nothing
t22_5' s = do
  v <- safeLookup "t22" s (BS.pack "pcq")
  case v of
    2 -> return $ Just $ \s -> do
      x <- safeLookup "t22" s (BS.pack "x")
      H.insert s (BS.pack "pcq") 3
      H.insert s (BS.pack "x2") x
      return s 
    _ -> return Nothing
t31_5' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    1 -> return $ Just $ \s -> do
      lock <- safeLookup "t31" s (BS.pack "lock")
      case lock of
        0 -> do
          H.insert s (BS.pack "pcr") 2
          H.insert s (BS.pack "lock") 1
          return s 
        1 -> do 
          H.insert s (BS.pack "pcr") 3
          return s 
    _ -> return Nothing
t32_5' s = do
  v <- safeLookup "t32" s (BS.pack "pcr")
  case v of
    2 -> return $ Just $ \s -> do
      x <- safeLookup "t32" s (BS.pack "x")
      H.insert s (BS.pack "pcr") 3
      H.insert s (BS.pack "x3") x
      return s 
    _ -> return Nothing


s5 :: ST s (Sigma s)
s5 = do 
  ht <- H.new
  H.insert ht (BS.pack "lock") 0 
  H.insert ht (BS.pack "x") 0 
  H.insert ht (BS.pack "pcp") 1 
  H.insert ht (BS.pack "pcq") 1 
  H.insert ht (BS.pack "pcr") 1 
  return ht

t1_5, t21_5,t22_5,t31_5,t32_5 :: Transition s
t1_5  = (BS.pack "p", 0, t1_5')
t21_5 = (BS.pack "q", 1, t21_5')
t22_5 = (BS.pack "q", 2, t22_5')
t31_5 = (BS.pack "r", 3, t31_5')
t32_5 = (BS.pack "r", 4, t32_5')

sys5 :: ST s (System s)
sys5 = do 
  is <- s5
  return $ System (V.fromList [t1_5,t21_5,t22_5,t31_5,t32_5]) is

-- [("t1","t2"),("t1","t4"),("t3","t5"),("t2","t5"),("t3","t4")]
ind5 :: UIndep
ind5 = V.generate 5 (\i -> V.generate 5 (\j -> check5 i j)) 

check5 :: Int -> Int -> Bool
check5 0 1 = True
check5 1 0 = True
check5 0 3 = True
check5 3 0 = True
check5 2 4 = True
check5 4 2 = True
check5 1 4 = True
check5 4 1 = True
check5 2 3 = True
check5 3 2 = True
check5 _ _ = False

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
