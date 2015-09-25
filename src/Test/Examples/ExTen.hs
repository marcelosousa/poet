module Test.Examples.ExTen (sys10, ind10) where

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

t21' :: TransitionFn Sigma
t21' s =
  let v = safeLookup "t21" s (BS.pack "pcq")
  in case v of
    IntVal 1 ->
      let ns = insert (BS.pack "x") (IntVal 2) s
          ns' = insert (BS.pack "pcq") (IntVal 2) ns
      in [ns']
    _ -> []

t21 :: Transition Sigma
t21 = ((BS.pack "q", 1, [Other]), t21')

t31' :: TransitionFn Sigma
t31' s =
  let v = safeLookup "t31" s (BS.pack "pcr")
  in case v of
    IntVal 1 ->
      let ns = insert (BS.pack "y") (IntVal 1) s
          ns' = insert (BS.pack "pcr") (IntVal 2) ns
      in [ns']
    _ -> []

t31 :: Transition Sigma
t31 = ((BS.pack "r", 2, [Other]), t31')

t41' :: TransitionFn Sigma
t41' s =
  let v = safeLookup "t41" s (BS.pack "pcs") 
  in case v of 
    IntVal 1 ->
      let ns = insert (BS.pack "y") (IntVal 2) s
          ns' = insert (BS.pack "pcs") (IntVal 2) ns
      in [ns']
    _ -> []

t41 :: Transition Sigma
t41 = ((BS.pack "s", 3, [Other]), t41')

s10 :: Sigma
s10 =
  let pairs = [(BS.pack "x", IntVal 0)
              ,(BS.pack "y", IntVal 0)
              ,(BS.pack "pcp", IntVal 1)
              ,(BS.pack "pcq", IntVal 1)
              ,(BS.pack "pcr", IntVal 1)
              ,(BS.pack "pcs", IntVal 1)]
  in toSigma pairs 
  
sys10 :: System Sigma
sys10 = System (V.fromList [t11,t21,t31,t41]) s10 [Other]

ind10 :: UIndep
ind10 = V.generate 4 (\i -> V.generate 4 (\j -> check i j))

check :: Int -> Int -> Bool
check 0 2 = True
check 2 0 = True
check 0 3 = True
check 3 0 = True
check 1 2 = True
check 2 1 = True
check 1 3 = True
check 3 1 = True
check _ _ = False
