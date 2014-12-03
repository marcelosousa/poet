module Model.Examples where

import Model.Language

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


putStrLn $ pretty $ dfsSimple example2 
[p@-1, q@-1] - x=2
[p@-1, q@1] - x=1
[p@-1, q@-1] - x=1
[p@0, q@-1] - x=2
[p@0, q@1] - x=0
-}
