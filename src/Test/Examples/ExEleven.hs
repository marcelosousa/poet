module Test.Examples.ExEleven (sys11, ind11) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

t11' :: TransitionFn Sigma
t11' s =
  let v = safeLookup "t11" s (BS.pack "pcp")
  in case v of
      IntVal 1 ->
        let ns = insert (BS.pack "x") (IntVal 1) s
            ns' = insert (BS.pack "pcp") (IntVal 2) ns
        in [ns']
      _ -> []

t11 :: Transition Sigma
t11 = ((BS.pack "p", 0, [Other]), t11')

t12' :: TransitionFn Sigma
t12' s =
  let v = safeLookup "t12" s (BS.pack "pcp")
  in case v of
      IntVal 2 ->
        let ns = insert (BS.pack "y") (IntVal 1) s
            ns' = insert (BS.pack "pcp") (IntVal 3) ns
        in [ns']
      _ -> []

t12 :: Transition Sigma
t12 = ((BS.pack "p", 1, [Other]), t12')

t21' :: TransitionFn Sigma
t21' s =
  let v = safeLookup "t21" s (BS.pack "pcq")
  in case v of
      IntVal 1 ->
        let ns = insert (BS.pack "y") (IntVal 2) s
            ns' = insert (BS.pack "pcq") (IntVal 2) ns
        in [ns']
      _ -> []

t21 :: Transition Sigma
t21 = ((BS.pack "q", 2, [Other]), t21')

t22' :: TransitionFn Sigma
t22' s =
  let v = safeLookup "t22" s (BS.pack "pcq")
  in case v of
      IntVal 2 ->
        let ns = insert (BS.pack "x") (IntVal 2) s
            ns' = insert (BS.pack "pcq") (IntVal 3) ns
        in [ns']
      _ -> []

t22 :: Transition Sigma
t22 = ((BS.pack "q", 3, [Other]), t22')

s11 :: Sigma
s11 =
  let pairs = [(BS.pack "x", IntVal 0)
              ,(BS.pack "y", IntVal 0)
              ,(BS.pack "pcp", IntVal 1)
              ,(BS.pack "pcq", IntVal 1)]
  in toSigma pairs 
  
sys11 :: System Sigma
sys11 = System (V.fromList [t11,t12,t21,t22]) s11 [Other]

ind11 :: UIndep
ind11 = V.generate 4 (\i -> V.generate 4 (\j -> check i j))

check :: Int -> Int -> Bool
check 0 2 = True
check 2 0 = True
check 1 3 = True
check 3 1 = True
check _ _ = False
