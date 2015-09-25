module Test.Examples.ExTwo where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

-- Example 2 - 1 write, 2 reads
s2 :: Sigma
s2 =
  let pairs = [(BS.pack "pcp", IntVal 1)
              ,(BS.pack "pcq", IntVal 1) 
              ,(BS.pack "pcr", IntVal 1)
              ,(BS.pack "x", IntVal 0)
              ,(BS.pack "l1", IntVal 0)
              ,(BS.pack "l2", IntVal 0)]
  in toSigma pairs

t1_2, t2_2, t3_2 :: Transition Sigma
t1_2 = ((BS.pack "p",0,[Other]),t1_2')
t2_2 = ((BS.pack "q",1,[Other]),t2_2')
t3_2 = ((BS.pack "r",2,[Other]),t3_2')

t1_2', t2_2', t3_2' :: TransitionFn Sigma
t1_2' s =
  let v = safeLookup "t1" s (BS.pack "pcp")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          xVal = (IntVal 1)
          ns = insert (BS.pack "pcp") pcVal s
          ns' = insert (BS.pack "x") xVal ns
      in [ns']
    _ -> []
t2_2' s =
  let v = safeLookup "t2" s (BS.pack "pcq")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          x = safeLookup "t2" s (BS.pack "x")
          ns = insert (BS.pack "pcq") pcVal s
          ns' = insert (BS.pack "l1") x ns
      in [ns']
    _ -> []
t3_2' s =
  let v = safeLookup "t3" s (BS.pack "pcr")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          x = safeLookup "t3" s (BS.pack "x")
          ns = insert (BS.pack "pcr") pcVal s
          ns' = insert (BS.pack "l2") x ns 
      in [ns']
    _ -> []

sys2 :: System Sigma
sys2 = System (V.fromList [t1_2,t2_2,t3_2]) s2 [Other]

ind2 :: UIndep
ind2 = V.generate 3 (\i -> V.generate 3 (\j -> check2 i j)) 
--ind2 = V.generate 3 (\i -> V.generate 3 (\j -> False)) 

check2 :: Int -> Int -> Bool
check2 1 2 = True
check2 2 1 = True
check2 _ _ = False