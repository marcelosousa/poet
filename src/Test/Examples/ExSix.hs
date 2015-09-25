module Test.Examples.ExSix where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

-- Example 6
s6 :: Sigma
s6 =
  let pairs = [(BS.pack "pcp", IntVal 1)
              ,(BS.pack "pcq", IntVal 1)
              ,(BS.pack "pcr", IntVal 1)
              ,(BS.pack "x", IntVal 0)  
              ,(BS.pack "x1", IntVal 0)  
              ,(BS.pack "x2", IntVal 0)]
  in toSigma pairs 

t1_6, t21_6, t22_6, t31_6, t32_6 :: Transition Sigma
t1_6  = ((BS.pack "p",0,[Other]),t1_6')
t21_6 = ((BS.pack "q",1,[Other]),t21_6')
t22_6 = ((BS.pack "q",2,[Other]),t22_6')
t31_6 = ((BS.pack "r",3,[Other]),t31_6')
t32_6 = ((BS.pack "r",4,[Other]),t32_6')

t1_6', t21_6', t22_6', t31_6', t32_6' :: TransitionFn Sigma
t1_6' s =
  let v = safeLookup "t1" s (BS.pack "pcp")
  in case v of
    IntVal 1 ->
      let x = safeLookup "t1" s (BS.pack "x")
          pcVal = (IntVal 2) 
          ns = insert (BS.pack "pcp") pcVal s
          ns' = insert (BS.pack "x1") x ns
      in [ns']
    _ -> []
t21_6' s =
  let v = safeLookup "t21" s (BS.pack "pcq")
  in case v of
    IntVal 1 ->
      let x = safeLookup "t1" s (BS.pack "x")
          pcVal = (IntVal 2)
          ns = insert (BS.pack "pcq") pcVal s
          ns' = insert (BS.pack "x2") x ns
      in [ns']
    _ -> []
t22_6' s =
  let v = safeLookup "t22" s (BS.pack "pcq")
  in case v of
    IntVal 2 ->
      let pcVal = (IntVal 3) 
          ns = insert (BS.pack "pcq") pcVal s
      in [ns]
    _ -> []
t31_6' s =
  let v = safeLookup "t31" s (BS.pack "pcr")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2) 
          ns = insert (BS.pack "pcr") pcVal s
      in [ns]
    _ -> []
t32_6' s =
  let v = safeLookup "t32" s (BS.pack "pcr")
  in case v of
    IntVal 2 ->
      let pcVal = (IntVal 3) 
          xVal = (IntVal 2)
          ns = insert (BS.pack "pcr") pcVal s
          ns' = insert (BS.pack "x") xVal ns
      in [ns']
    _ -> []

sys6 :: System Sigma
sys6 = System (V.fromList [t1_6,t21_6,t22_6,t31_6,t32_6]) s6 [Other]

ind6 :: UIndep
ind6 = V.generate 5 (\i -> V.generate 5 (\j -> check6 i j)) 

check6 :: Int -> Int -> Bool
-- R R
check6 0 1 = True
check6 1 0 = True
-- R K
check6 0 2 = True
check6 2 0 = True
-- R K
check6 0 3 = True
check6 3 0 = True
-- R K
check6 1 3 = True
check6 3 1 = True
-- K W
check6 2 4 = True
check6 4 2 = True
-- R K
check6 2 3 = True
check6 3 2 = True
check6 _ _ = False