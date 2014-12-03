module Examples where

import Model 
import qualified Data.Map as M
import Data.Maybe

-- Example 1
t1' :: Sigma -> Maybe Sigma
t1' s = case M.lookup "pcp" s of
    Nothing -> Nothing
    Just 1  -> 
      let s' = M.insert "x" 1 $ M.insert "pcp" 2 s
      in Just s'
    Just _  -> Nothing
        
t1 :: Transition
t1 = ("p", "t1", t1')

t2' :: Sigma -> Maybe Sigma
t2' s = case M.lookup "pcq" s of
    Nothing -> Nothing
    Just 1  -> 
      let s' = M.insert "y" 1 $ M.insert "pcq" 2 s
      in Just s'
    Just _  -> Nothing

t2 :: Transition
t2 = ("q", "t2", t2')

s1 :: Sigma
s1 = M.insert "pcp" 1 $ M.singleton "pcq" 1

sys1 :: System 
sys1 = ([t1,t2], s1)

ind1 :: UIndependence
ind1 = [] --("t1","t2")]

-- Example 2 - paper
s2 :: Sigma
s2 = M.fromList [("pcp",1),("pcq",1),("pcr",1),("pcs",1),("x",0),("y",0),("z",0)]

t1_2, t2_2, t31_2, t32_2, t41_2, t42_2 :: Transition
t1_2 = ("p","t1",t1_2')
t2_2 = ("q","t2",t2_2')
t31_2 = ("r","t31",t31_2')
t32_2 = ("r","t32",t32_2')
t41_2 = ("s","t41",t41_2')
t42_2 = ("s","t42",t42_2')

t1_2', t2_2', t31_2', t32_2', t41_2', t42_2' :: Sigma -> Maybe Sigma
t1_2' s = 
    case M.lookup "pcp" s of
        Nothing -> Nothing
        Just 1  -> 
          let s' = M.insert "x" 1 $ M.insert "pcp" 2 s
          in Just s'
        Just _  -> Nothing
t2_2' s =
    case M.lookup "pcq" s of
        Nothing -> Nothing
        Just 1  -> 
          let s' = M.insert "y" 1 $ M.insert "pcq" 2 s
          in Just s'
        Just _  -> Nothing
t31_2' s =
    case M.lookup "pcr" s of
        Nothing -> Nothing
        Just 1  -> 
          case M.lookup "y" s of
              Nothing -> error "should not happen"
              Just 0  -> Just $ M.insert "pcr" 2 s
              Just _  -> Just $ M.insert "pcr" 3 s
        Just _  -> Nothing
t32_2' s =
    case M.lookup "pcr" s of
        Nothing -> Nothing
        Just 2  -> 
          let s' = M.insert "z" 1 $ M.insert "pcr" 3 s
          in Just s'
        Just _  -> Nothing
t41_2' s =
    case M.lookup "pcs" s of
        Nothing -> Nothing
        Just 1  -> 
          case M.lookup "z" s of
              Nothing -> error "should not happen"
              Just 1  -> Just $ M.insert "pcs" 2 s
              Just _  -> Just $ M.insert "pcs" 3 s
        Just _  -> Nothing
t42_2' s =
    case M.lookup "pcs" s of
        Nothing -> Nothing
        Just 2  -> 
          let s' = M.insert "x" 1 $ M.insert "pcs" 3 s
          in Just s'
        Just _  -> Nothing

sys2 :: System 
sys2 = ([t1_2, t2_2, t31_2, t32_2, t41_2, t42_2 ], s2)

ind2 :: UIndependence
ind2 = [("t1","t2"),("t1","t31"),("t1","t32"),("t1","t41"),("t2","t32"),("t2","t41"),("t2","t42"),("t31","t41"),("t31","t42"),("t32","t42")]


-- Example 3 - 1 write, 2 reads
s3 :: Sigma
s3 = M.fromList [("pcp",1),("pcq",1),("pcr",1),("x",0),("l1",0),("l2",0)]

t1_3, t2_3, t3_3 :: Transition
t1_3 = ("p","t1",t1_3')
t2_3 = ("q","t2",t2_3')
t3_3 = ("r","t3",t3_3')
--t32_3 = ("r","t32",t32_3')

t1_3', t2_3', t3_3' :: Sigma -> Maybe Sigma
t1_3' s = 
    case M.lookup "pcp" s of
        Nothing -> Nothing
        Just 1  -> 
          let s' = M.insert "x" 1 $ M.insert "pcp" 2 s
          in Just s'
        Just _  -> Nothing
t2_3' s =
    case M.lookup "pcq" s of
        Nothing -> Nothing
        Just 1  -> case M.lookup "x" s of
            Nothing -> error ""
            Just k -> 
              let s' = M.insert "l1" k $ M.insert "pcq" 2 s
              in Just s'
        Just _  -> Nothing
t3_3' s =
    case M.lookup "pcr" s of
        Nothing -> Nothing
        Just 1  -> case M.lookup "x" s of
            Nothing -> error ""
            Just k -> 
              let s' = M.insert "l2" k $ M.insert "pcr" 2 s
              in Just s'
        Just _  -> Nothing
--t32_3' s =
--    case M.lookup "pcr" s of
--        Nothing -> Nothing
--        Just 2  -> 
--          let s' = M.insert "x" 1 $ M.insert "pcr" 3 s
--          in Just s'
--        Just _  -> Nothing

sys3 :: System 
sys3 = ([t1_3, t2_3, t3_3], s3)

ind3 :: UIndependence
ind3 = [("t2","t3")]

-- Example 4
t11_4' :: Sigma -> Maybe Sigma
t11_4' s = case M.lookup "pcp" s of
    Nothing -> Nothing
    Just 1  -> 
      let s' = M.insert "y" 1 $ M.insert "pcp" 2 s
      in Just s'
    Just _  -> Nothing

t12_4' :: Sigma -> Maybe Sigma
t12_4' s = case M.lookup "pcp" s of
    Nothing -> Nothing
    Just 2  -> 
      let s' = M.insert "x" 1 $ M.insert "pcp" 3 s
      in Just s'
    Just _  -> Nothing
        
t11_4 :: Transition
t11_4 = ("p", "t1", t11_4')
t12_4 :: Transition
t12_4 = ("p", "t2", t12_4')

t21_4' :: Sigma -> Maybe Sigma
t21_4' s = case M.lookup "pcq" s of
    Nothing -> Nothing
    Just 1  -> 
      let s' = M.insert "z" 1 $ M.insert "pcq" 2 s
      in Just s'
    Just _  -> Nothing

t22_4' :: Sigma -> Maybe Sigma
t22_4' s = case M.lookup "pcq" s of
    Nothing -> Nothing
    Just 2  -> 
      let s' = M.insert "x" 2 $ M.insert "pcq" 3 s
      in Just s'
    Just _  -> Nothing

t21_4 :: Transition
t21_4 = ("q", "t3", t21_4')
t22_4 :: Transition
t22_4 = ("q", "t4", t22_4')

s4 :: Sigma
s4 = M.insert "pcp" 1 $ M.singleton "pcq" 1

sys4 :: System 
sys4 = ([t11_4,t12_4,t21_4,t22_4], s4)

ind4 :: UIndependence
ind4 = [("t1","t3"),("t1","t4"),("t2","t3")]

-- Example 5 - Cesar's example
t1_5',t21_5',t22_5',t31_5',t32_5' :: Sigma -> Maybe Sigma
t1_5' s = case M.lookup "pcp" s of
    Nothing -> Nothing
    Just 1  -> 
      let s' = M.insert "x" 1 $ M.insert "pcp" 2 s
      in Just s'
    Just _  -> Nothing
t21_5' s = case M.lookup "pcq" s of
    Nothing -> Nothing
    Just 1  -> case M.lookup "lock" s of
      Nothing -> error "can't happen"
      Just 0  -> Just $ M.insert "lock" 1 $ M.insert "pcq" 2 s
      Just 1  -> Just $ M.insert "pcq" 3 s
    Just _  -> Nothing
t22_5' s = case M.lookup "pcq" s of
    Nothing -> Nothing
    Just 2  -> case M.lookup "x" s of
      Nothing -> error "cant happen"
      Just x  -> 
        let s' = M.insert "x2" x $ M.insert "pcq" 3 s
        in Just s'
    Just _  -> Nothing
t31_5' s = case M.lookup "pcr" s of
    Nothing -> Nothing
    Just 1  -> case M.lookup "lock" s of
      Nothing -> error "can't happen"
      Just 0  -> Just $ M.insert "lock" 1 $ M.insert "pcr" 2 s
      Just 1  -> Just $ M.insert "pcr" 3 s
    Just _  -> Nothing
t32_5' s = case M.lookup "pcr" s of
    Nothing -> Nothing
    Just 2  -> case M.lookup "x" s of
      Nothing -> error "cant happen"
      Just x  -> 
        let s' = M.insert "x3" x $ M.insert "pcr" 3 s
        in Just s'
    Just _  -> Nothing
        
t1_5, t21_5,t22_5,t31_5,t32_5 :: Transition
t1_5 = ("p", "t1", t1_5')
t21_5 = ("q", "t2", t21_5')
t22_5 = ("q", "t3", t22_5')
t31_5 = ("r", "t4", t31_5')
t32_5 = ("r", "t5", t32_5')

s5 :: Sigma
s5 = M.insert "x" 0 $ M.insert "lock" 0 $ M.insert "pcp" 1 $ M.insert "pcq" 1 $ M.singleton "pcr" 1

sys5 :: System 
sys5 = ([t1_5,t21_5,t22_5,t31_5,t32_5], s5)

ind5 :: UIndependence
ind5 = [("t1","t2"),("t1","t4"),("t3","t5"),("t2","t5"),("t3","t4")]

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

{-    
p :: Transition
p (0, e) = let e' = M.adjust (const 1) "x" e
           in  (-1, e')
p _ = undefined

q :: Transition
q (1, e) = let e' = M.adjust (const 1) "y" e
           in  (-1, e')
q _ = undefined

r :: Transition
r (2, e) = 
    let y = slookup "y" e
        e' = M.insert "m" y e
    in (3, e')
r (3, e) = 
    let m = slookup "m" e
        e' = M.adjust (const 1) "z" e
    in if m == 0
       then (-1, e')
       else (-1, e)
r _ = undefined

s :: Transition
s (4, e) =
    let z = slookup "z" e
        e' = M.insert "n" z e
    in (5, e')
s (5, e) = 
    let y = slookup "y" e    
        e' = M.insert "l" y e
    in (6, e')
s (6, e) = 
    let n = slookup "n" e
        l = slookup "l" e
        e' = M.adjust (const 2) "x" e
    in if n == 1 && l == 0
       then (-1, e')
       else (-1, e)
s _ = undefined

tr :: PTransition
tr = M.fromList [("p",p), ("q",q), ("r",r), ("s",s)]
--tr = [p,q,r,s]

i :: State
i = let pcs = M.fromList [("p",0), ("q",1), ("r",2), ("s",4)]
        ival = M.fromList [("x",0),("y",0),("z",0)]
    in (pcs, ival)

example :: System
example = System tr i

-- Simpler example 
-- Two threads 
-- Thread 1: x = 1
-- Thread 2: x = 2
t1 :: Transition
t1 (0, e) = let e' = M.adjust (const 1) "x" e
           in  (-1, e')
t1 _ = undefined

t2 :: Transition
t2 (1, e) = let e' = M.adjust (const 2) "x" e
           in  (-1, e')
t2 _ = undefined

tr2 :: PTransition
tr2 = M.fromList [("p",t1),("q",t2)]

i2 :: State
i2 = let pcs = M.fromList [("p",0),("q",1)]
         ival = M.fromList [("x",0)]
     in (pcs,ival)

example2 :: System
example2 = System tr2 i2
-}
