module Test.Examples.ExNine (sys9, ind9) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Domain.Concrete.Type
import Model.GCS
import Model.Independence
import Util.Generic hiding (safeLookup)

t11' :: TransitionFn Sigma
t11' s = [insert (BS.pack "x") (IntVal 1) s]

t11 :: Transition Sigma
t11 = ((BS.pack "m", 0, [Other]), t11')

s9 :: Sigma
s9 = toSigma [(BS.pack "x", IntVal 0)] 
  
sys9 :: System Sigma
sys9 = System (V.fromList [t11]) s9 [Other]

ind9 :: UIndep
ind9 = V.generate 1 (\i -> V.generate 1 (\j -> False)) 
