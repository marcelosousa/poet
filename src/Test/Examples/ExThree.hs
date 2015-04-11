module Test.Examples.ExThree where

import Model.GCS

import Control.Monad.ST.Safe

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.Class as H
import qualified Data.Vector as V

-- Example 3 - paper
s3 :: ST s (Sigma s)
s3 = do 
  ht <- H.new
  H.insert ht (BS.pack "pcp") (IntVal 1) 
  H.insert ht (BS.pack "pcq") (IntVal 1) 
  H.insert ht (BS.pack "pcr") (IntVal 1) 
  H.insert ht (BS.pack "pcs") (IntVal 1) 
  H.insert ht (BS.pack "x") (IntVal 0)  
  H.insert ht (BS.pack "y") (IntVal 0)  
  H.insert ht (BS.pack "z") (IntVal 0)  
  return ht

t1_3, t2_3, t31_3, t32_3, t41_3, t42_3 :: Transition s
t1_3 = (BS.pack "p",0,[Other],t1_3')
t2_3 = (BS.pack "q",1,[Other],t2_3')
t31_3 = (BS.pack "r",2,[Other],t31_3')
t32_3 = (BS.pack "r",3,[Other],t32_3')
t41_3 = (BS.pack "s",4,[Other],t41_3')
t42_3 = (BS.pack "s",5,[Other],t42_3')

t1_3', t2_3', t31_3', t32_3', t41_3', t42_3' :: TransitionFn s 
t1_3' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      let pcVal = (IntVal 2)
          xVal = (IntVal 1) 
      H.insert s (BS.pack "pcp") pcVal
      H.insert s (BS.pack "x") xVal
      return s
    _ -> return Nothing
t2_3' s = do
  v <- safeLookup "t2" s (BS.pack "pcq")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      let pcVal = (IntVal 2)
          yVal = (IntVal 1) 
      H.insert s (BS.pack "pcq") pcVal
      H.insert s (BS.pack "y") yVal
      return s
    _ -> return Nothing
t31_3' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      (IntVal y) <- safeLookup "t31" s (BS.pack "y")
      let pcr = if y == 0 then 2 else 3
          pcVal = (IntVal pcr)
      H.insert s (BS.pack "pcr") pcVal
      return s
    _ -> return Nothing
t32_3' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 2) -> return $ Just $ \s -> do
      let pcVal = (IntVal 3)
          zVal = (IntVal 1) 
      H.insert s (BS.pack "pcr") pcVal
      H.insert s (BS.pack "z") zVal
      return s
    _ -> return Nothing
t41_3' s = do
  v <- safeLookup "t41" s (BS.pack "pcs")
  case v of
    (IntVal 1) -> return $ Just $ \s -> do
      (IntVal y) <- safeLookup "t41" s (BS.pack "z")
      let pcs = if y == 1 then 2 else 3
          pcVal =  (IntVal pcs)
      H.insert s (BS.pack "pcs") pcVal
      return s
    _ -> return Nothing
t42_3' s = do
  v <- safeLookup "t42" s (BS.pack "pcs")
  case v of
    (IntVal 2) -> return $ Just $ \s -> do
      let pcVal = (IntVal 3) 
          xVal = (IntVal 2) 
      H.insert s (BS.pack "pcs") pcVal
      H.insert s (BS.pack "x") xVal
      return s
    _ -> return Nothing

sys3 :: ST s (System s)
sys3 = do 
  is <- s3
  return $ System (V.fromList [t1_3,t2_3,t31_3,t32_3,t41_3,t42_3]) is [Other]

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