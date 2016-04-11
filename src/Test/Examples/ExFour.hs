module Test.Examples.ExFour where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

-- Example 4
s4 :: Sigma
s4 =
  let pairs = [(BS.pack "pcp", IntVal 1)
              ,(BS.pack "pcq", IntVal 1)
              ,(BS.pack "x", IntVal 0)  
              ,(BS.pack "y", IntVal 0)  
              ,(BS.pack "z", IntVal 0)]
  in toSigma pairs 

t11_4, t12_4, t21_4, t22_4 :: Transition Sigma
t11_4 = ((BS.pack "p",0,[],[Other]),t11_4')
t12_4 = ((BS.pack "p",1,[],[Other]),t12_4')
t21_4 = ((BS.pack "q",2,[],[Other]),t21_4')
t22_4 = ((BS.pack "q",3,[],[Other]),t22_4')

t11_4', t12_4', t21_4', t22_4' :: TransitionFn Sigma
t11_4' s =
  let v = safeLookup "t11" s (BS.pack "pcp")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          yVal = (IntVal 1)
          ns = insert (BS.pack "pcp") pcVal s
          ns' = insert (BS.pack "y") yVal ns
      in [ns']
    _ -> []
t12_4' s =
  let v = safeLookup "t12" s (BS.pack "pcp")
  in case v of
    IntVal 2 ->
      let pcVal = (IntVal 3)
          xVal = (IntVal 1)
          ns = insert (BS.pack "pcp") pcVal s
          ns' = insert (BS.pack "x") xVal ns
      in [ns']
    _ -> []
t21_4' s =
  let v = safeLookup "t21" s (BS.pack "pcq")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          zVal = (IntVal 1)
          ns = insert (BS.pack "pcq") pcVal s
          ns' = insert (BS.pack "z") zVal ns
      in [ns']
    _ -> []
t22_4' s =
  let v = safeLookup "t22" s (BS.pack "pcq")
  in case v of
    IntVal 2 ->
      let pcVal = (IntVal 3)
          xVal = (IntVal 2)
          ns = insert (BS.pack "pcq") pcVal s
          ns' = insert (BS.pack "x") xVal ns
      in [ns']
    _ -> []

sys4 :: System Sigma
sys4 = System (V.fromList [t11_4,t12_4,t21_4,t22_4]) s4 [Other]

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