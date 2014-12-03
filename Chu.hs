module Chu where

-- Set of points
type A = [Int]

-- Set of states
type X = [Int]

-- k-Relation
type K = Int
type R = A -> X -> K

data ChuSpace = ChuSpace (A,R,X)
--  deriving (Eq,Ord)
  
instance Show ChuSpace where
  show (ChuSpace (a,r,x)) = ""
  
fibpair :: Int -> (Int, Int)
fibpair 0 = (0,1)
fibpair n = (\z -> (snd z, (fst z + snd z))) (fibpair (n-1))
--fibpair n = (j, i+j) where (i,j) = fibpair (n-1)

fib :: Int -> Int
fib = fst . fibpair
