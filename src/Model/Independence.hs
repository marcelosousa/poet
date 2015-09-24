module Model.Independence where

import qualified Data.Vector as V
import Model.GCS

-- An independence relation is an irreflexive and symmetric relation
-- Triangular Matrix to exploit symmetry of independence relation
-- The irreflexivity can be simply checked with equality which is O(1) 
type UIndep = V.Vector (V.Vector Bool)

-- It is many times the case that we run mutually exclusive functions
-- like what i have below (min, max). Program consolidation could 
-- optimisize this code.
-- | isIndependent -- check if two transitions are uncond. indep.
isIndependent, isDependent :: UIndep -> TransitionInfo -> TransitionInfo -> Bool
isIndependent uindep (p1,t1,_) (p2,t2,_)  
  | (t1 == botID) || (t2 == botID) || (t1 == t2) || (p1 == p2)  = False
  | otherwise = 
      let t  = min t1 t2
          t' = max t1 t2
      in uindep V.! t V.! t'
 
-- | isDependent - checks if two transitions are dependent
isDependent uindep t1 t2 = not $ isIndependent uindep t1 t2

getIndepTr :: UIndep -> [TransitionInfo] -> [(TransitionInfo,TransitionInfo)]
getIndepTr uindep trs =
    [ (t1,t2) | t1 <- trs, t2 <- trs, t1 < t2 && isIndependent uindep t1 t2]

