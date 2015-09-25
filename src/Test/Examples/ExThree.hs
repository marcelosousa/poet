module Test.Examples.ExThree where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

-- Example 3 - paper
s3 :: Sigma
s3 =
  let pairs = [(BS.pack "pcp", IntVal 1)
              ,(BS.pack "pcq", IntVal 1)
              ,(BS.pack "pcr", IntVal 1)
              ,(BS.pack "pcs", IntVal 1)
              ,(BS.pack "x", IntVal 0)  
              ,(BS.pack "y", IntVal 0)  
              ,(BS.pack "z", IntVal 0)]
  in toSigma pairs 
  
t1_3, t2_3, t31_3, t32_3, t41_3, t42_3 :: Transition Sigma
t1_3  = ((BS.pack "p",0,[Other]),t1_3')
t2_3  = ((BS.pack "q",1,[Other]),t2_3')
t31_3 = ((BS.pack "r",2,[Other]),t31_3')
t32_3 = ((BS.pack "r",3,[Other]),t32_3')
t41_3 = ((BS.pack "s",4,[Other]),t41_3')
t42_3 = ((BS.pack "s",5,[Other]),t42_3')

t1_3', t2_3', t31_3', t32_3', t41_3', t42_3' :: TransitionFn Sigma 
t1_3' s =
  let v = safeLookup "t1" s (BS.pack "pcp")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          xVal = (IntVal 1) 
          ns = insert (BS.pack "pcp") pcVal s
          ns' = insert (BS.pack "x") xVal ns
      in [ns']
    _ -> []
t2_3' s =
  let v = safeLookup "t2" s (BS.pack "pcq")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          yVal = (IntVal 1)
          ns = insert (BS.pack "pcq") pcVal s
          ns' = insert (BS.pack "y") yVal ns
      in [ns']
    _ -> []
t31_3' s =
  let v = safeLookup "t31" s (BS.pack "pcr")
  in case v of
    IntVal 1 ->
      let IntVal y = safeLookup "t31" s (BS.pack "y")
          pcr = if y == 0 then 2 else 3
          pcVal = (IntVal pcr)
          ns = insert (BS.pack "pcr") pcVal s
      in [ns]
    _ -> []
t32_3' s =
  let v = safeLookup "t31" s (BS.pack "pcr")
  in case v of
    IntVal 2 ->
      let pcVal = (IntVal 3)
          zVal = (IntVal 1) 
          ns = insert (BS.pack "pcr") pcVal s
          ns' = insert (BS.pack "z") zVal ns
      in [ns']
    _ -> []
t41_3' s =
  let v = safeLookup "t41" s (BS.pack "pcs")
  in case v of
    IntVal 1 ->
      let IntVal y = safeLookup "t41" s (BS.pack "z")
          pcs = if y == 1 then 2 else 3
          pcVal =  (IntVal pcs)
          ns = insert (BS.pack "pcs") pcVal s
      in [ns]
    _ -> []
t42_3' s =
  let v = safeLookup "t42" s (BS.pack "pcs")
  in case v of
    IntVal 2 ->
      let pcVal = (IntVal 3) 
          xVal = (IntVal 2)
          ns = insert (BS.pack "pcs") pcVal s
          ns' = insert (BS.pack "x") xVal ns
      in [ns']
    _ -> []

sys3 :: System Sigma
sys3 = System (V.fromList [t1_3,t2_3,t31_3,t32_3,t41_3,t42_3]) s3 [Other]

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