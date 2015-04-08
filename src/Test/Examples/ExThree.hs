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
  H.insert ht (BS.pack "pcp") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "pcq") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "pcr") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "pcs") (IntVal 1, Nothing) 
  H.insert ht (BS.pack "x") (IntVal 0, Nothing)  
  H.insert ht (BS.pack "y") (IntVal 0, Nothing)  
  H.insert ht (BS.pack "z") (IntVal 0, Nothing)  
  return ht

t1_3, t2_3, t31_3, t32_3, t41_3, t42_3 :: Transition s
t1_3 = (BS.pack "p",0,t1_3')
t2_3 = (BS.pack "q",1,t2_3')
t31_3 = (BS.pack "r",2,t31_3')
t32_3 = (BS.pack "r",3,t32_3')
t41_3 = (BS.pack "s",4,t41_3')
t42_3 = (BS.pack "s",5,t42_3')

t1_3', t2_3', t31_3', t32_3', t41_3', t42_3' :: TransitionFn s 
t1_3' s = do
  v <- safeLookup "t1" s (BS.pack "pcp")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcp") (IntVal 2, Nothing) 
      H.insert s (BS.pack "x") (IntVal 1, Nothing) 
      return (s,[(BS.pack "pcp", IntVal 2),(BS.pack "x", IntVal 1)]) 
    _ -> return Nothing
t2_3' s = do
  v <- safeLookup "t2" s (BS.pack "pcq")
  case v of
    (IntVal 1, _) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcq") (IntVal 2, Nothing) 
      H.insert s (BS.pack "y") (IntVal 1, Nothing) 
      return (s,[(BS.pack "pcq", IntVal 2),(BS.pack "y", IntVal 1)]) 
    _ -> return Nothing
t31_3' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      (IntVal y,_) <- safeLookup "t31" s (BS.pack "y")
      let pcr = if y == 0 then 2 else 3
      H.insert s (BS.pack "pcr") (IntVal pcr, Nothing)
      return (s,[(BS.pack "pcr", IntVal pcr)]) 
    _ -> return Nothing
t32_3' s = do
  v <- safeLookup "t31" s (BS.pack "pcr")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcr") (IntVal 3, Nothing) 
      H.insert s (BS.pack "z") (IntVal 1, Nothing) 
      return (s,[(BS.pack "pcr", IntVal 3),(BS.pack "z", IntVal 1)]) 
    _ -> return Nothing
t41_3' s = do
  v <- safeLookup "t41" s (BS.pack "pcs")
  case v of
    (IntVal 1,_) -> return $ Just $ \s -> do
      (IntVal y,_) <- safeLookup "t41" s (BS.pack "z")
      let pcs = if y == 1 then 2 else 3
      H.insert s (BS.pack "pcs") (IntVal pcs, Nothing)
      return (s,[(BS.pack "pcs", IntVal pcs)]) 
    _ -> return Nothing
t42_3' s = do
  v <- safeLookup "t42" s (BS.pack "pcs")
  case v of
    (IntVal 2,_) -> return $ Just $ \s -> do
      H.insert s (BS.pack "pcs") (IntVal 3, Nothing) 
      H.insert s (BS.pack "x") (IntVal 2, Nothing) 
      return (s,[(BS.pack "pcs", IntVal 3),(BS.pack "x", IntVal 2)]) 
    _ -> return Nothing

sys3 :: ST s (System s)
sys3 = do 
  is <- s3
  lis <- H.toList is >>= return . map (\(a,b) -> (a, fst b))
  return $ System (V.fromList [t1_3,t2_3,t31_3,t32_3,t41_3,t42_3]) is lis

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