module Test.Examples.ExEight (sys8, ind8) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

-- Example 8 - Lock example
t11',t12',t21',t22',t23' :: TransitionFn Sigma
t11' s =
  let v = safeLookup "t11" s (BS.pack "pcq")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          ns = insert (BS.pack "pcq") pcVal s
          ns' = insert (BS.pack "lock") (IntVal 0) ns
      in [ns']
    _ -> []

t12' s =
  let v = safeLookup "t12" s (BS.pack "pcq")
  in case v of
    IntVal 2 ->
      let ns = insert (BS.pack "pcq") (IntVal 3) s
          ns' = insert (BS.pack "x") (IntVal 1) ns
      in [ns']
    _ -> []

t21' s =
  let IntVal v = safeLookup "t21" s (BS.pack "pcr")
      IntVal lock = safeLookup "t21" s (BS.pack "lock")
  in if v == 1 && lock == 0
     then
       let pcVal = (IntVal 2)
           lockVal = (IntVal 1)
           ns = insert (BS.pack "pcr") pcVal s
           ns' = insert (BS.pack "lock") lockVal ns
       in [ns']
     else []
t22' s =
  let v = safeLookup "t22" s (BS.pack "pcr")
  in case v of
    IntVal 2 ->
      let ns = insert (BS.pack "pcr") (IntVal 3) s
          ns' = insert (BS.pack "x") (IntVal 2) ns
      in [ns']
    _ -> []
t23' s =
  let v = safeLookup "t23" s (BS.pack "pcr")
  in case v of
    IntVal 3 ->
      let pcVal = (IntVal 4)
          ns = insert (BS.pack "pcr") pcVal s
          ns' = insert (BS.pack "lock") (IntVal 0) ns
      in [ns']
    _ -> []

s8 :: Sigma
s8 =
  let pairs = [(BS.pack "pcq", IntVal 1)
              ,(BS.pack "pcr", IntVal 1)
              ,(BS.pack "x", IntVal 0)  
              ,(BS.pack "lock", IntVal 1)]
  in toSigma pairs 

t11,t12,t21,t22,t23 :: Transition Sigma
t11 = ((BS.pack "q", 0, [Unlock $ V $ BS.pack "lock"]), t11')
t12 = ((BS.pack "q", 1, [Other]), t12')
t21 = ((BS.pack "r", 2, [Lock $ V $ BS.pack "lock"]), t21')
t22 = ((BS.pack "r", 3, [Other]), t22')
t23 = ((BS.pack "r", 4, [Unlock $ V $ BS.pack "lock"]), t23')

sys8 :: System Sigma
sys8 = System (V.fromList [t11,t12,t21,t22,t23]) s8 [Lock $ V $ BS.pack "lock"]

ind8 :: UIndep
ind8 = V.generate 5 (\i -> V.generate 5 (\j -> check i j)) 

check :: Int -> Int -> Bool
check 0 3 = True
check 3 0 = True
check 1 2 = True
check 2 1 = True
check 1 4 = True
check 4 1 = True
check _ _ = False
