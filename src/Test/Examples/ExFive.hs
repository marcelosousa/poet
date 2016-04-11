module Test.Examples.ExFive where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

-- Example 5 - Cesar's example
t1_5',t21_5',t22_5',t31_5',t32_5' :: TransitionFn Sigma
t1_5' s =
  let v = safeLookup "t1" s (BS.pack "pcp")
  in case v of
    IntVal 1 ->
      let pcVal = (IntVal 2)
          xVal = (IntVal 1)
          ns = insert (BS.pack "pcp") pcVal s
          ns' = insert (BS.pack "x") xVal ns
      in [ns']
    _ -> []
t21_5' s =
  let v = safeLookup "t21" s (BS.pack "pcq")
  in case v of
    IntVal 1 ->
      let IntVal lock = safeLookup "t21" s (BS.pack "lock")
      in if lock == 0
         then
           let pcVal = (IntVal 2)
               lockVal = (IntVal 1)
               ns = insert (BS.pack "pcq") pcVal s
               ns' = insert (BS.pack "lock") lockVal ns
           in [ns']
         else
           let pcVal = (IntVal 3)
               ns = insert (BS.pack "pcq") pcVal s
           in [ns]
    _ -> []
t22_5' s =
  let v = safeLookup "t22" s (BS.pack "pcq")
  in case v of
    IntVal 2 ->
      let x = safeLookup "t22" s (BS.pack "x")
          pcVal = (IntVal 3)
          ns = insert (BS.pack "pcq") pcVal s
          ns' = insert (BS.pack "x2") x ns
      in [ns']
    _ -> []
t31_5' s =
  let v = safeLookup "t31" s (BS.pack "pcr")
  in case v of
    IntVal 1 ->
      let IntVal lock = safeLookup "t31" s (BS.pack "lock")
      in if lock == 0
         then
           let pcVal = (IntVal 2)
               lockVal = (IntVal 1)
               ns = insert (BS.pack "pcr") pcVal s
               ns' = insert (BS.pack "lock") lockVal ns
           in [ns']
         else
           let pcVal = (IntVal 3)
               ns = insert (BS.pack "pcr") pcVal s
           in [ns]
    _ -> []
t32_5' s =
  let v = safeLookup "t32" s (BS.pack "pcr")
  in case v of
    IntVal 2 ->
      let x = safeLookup "t32" s (BS.pack "x")
          pcVal = (IntVal 3)
          ns = insert (BS.pack "pcr") pcVal s
          ns' = insert (BS.pack "x3") x ns
      in [ns']
    _ -> []

s5 :: Sigma
s5 =
  let pairs = [(BS.pack "pcp", IntVal 1)
              ,(BS.pack "pcq", IntVal 1)
              ,(BS.pack "pcr", IntVal 1)
              ,(BS.pack "x", IntVal 0)  
              ,(BS.pack "x2", IntVal 0)  
              ,(BS.pack "x3", IntVal 0)
              ,(BS.pack "lock", IntVal 0)]
  in toSigma pairs 

t1_5, t21_5,t22_5,t31_5,t32_5 :: Transition Sigma
t1_5  = ((BS.pack "p", 0, [], [Other]), t1_5')
t21_5 = ((BS.pack "q", 1, [], [Other]), t21_5')
t22_5 = ((BS.pack "q", 2, [], [Other]), t22_5')
t31_5 = ((BS.pack "r", 3, [], [Other]), t31_5')
t32_5 = ((BS.pack "r", 4, [], [Other]), t32_5')

sys5 :: System Sigma
sys5 = System (V.fromList [t1_5,t21_5,t22_5,t31_5,t32_5]) s5 [Other]

-- [("t1","t2"),("t1","t4"),("t3","t5"),("t2","t5"),("t3","t4")]
ind5 :: UIndep
ind5 = V.generate 5 (\i -> V.generate 5 (\j -> check5 i j)) 

check5 :: Int -> Int -> Bool
check5 0 1 = True
check5 1 0 = True
check5 0 3 = True
check5 3 0 = True
check5 2 4 = True
check5 4 2 = True
check5 1 4 = True
check5 4 1 = True
check5 2 3 = True
check5 3 2 = True
check5 _ _ = False
