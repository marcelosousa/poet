module Test.Examples.ExOne where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)
import Language.SimpleC.AST

-- Example 1 - Two writes of different variables
-- Sigma s -> ST s (Maybe (Sigma s -> ST s (Sigma s)))
t1' :: TransitionFn Sigma
t1' s =
  let v = safeLookup "t1" s (BS.pack "pcp")
  in case v of
    IntVal 1 ->
      let ns = insert (BS.pack "pcp") (IntVal 2) s
          ns' = insert (BS.pack "x") (IntVal 1) ns
      in [ns']
    _ -> []

t1 :: Transition Sigma
t1 = ((BS.pack "p", 0, [], [Other]), t1')

t2' :: TransitionFn Sigma
t2' s =
  let v = safeLookup "t2" s (BS.pack "pcq")
  in case v of
    IntVal 1 ->
      let ns = insert (BS.pack "pcq") (IntVal 2) s
          ns' = insert (BS.pack "y") (IntVal 1) ns
      in [ns']
    _ -> []

t2 :: Transition Sigma
t2 = ((BS.pack "q", 1, [],[Other]), t2')

s1 :: Sigma
s1 = 
  let pairs = [(BS.pack "pcp",IntVal 1)
              ,(BS.pack "pcq",IntVal 1)
              ,(BS.pack "x",IntVal 0)
              ,(BS.pack "y",IntVal 0)] 
  in toSigma pairs

sys1 :: System Sigma
sys1 = System (V.fromList [t1,t2]) s1 [Other]

ind11,ind12 :: UIndep
ind11 = V.generate 2 (\i -> V.generate 2 (\j -> False)) 
ind12 = V.generate 2 (\i -> V.generate 2 (\j -> if i /= j then True else False)) 

