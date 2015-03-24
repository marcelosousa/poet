{-#LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Exploration.Auxiliary where

import Model.Language

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List

-- For an execution sequence E.w, let I[E](w) 
-- denote the set of processes that perform 
-- events e in dom[E](w) that have no “happens-before” 
-- predecessors in dom[E](w).
minIndep :: Execution -> Events -> [Var]
minIndep _ [] = []
minIndep eseq (e:es) = minIndep' e es ++ minIndep eseq es
    where
      minIndep' :: Event -> Events -> [Var]
      minIndep' e@(p,i,tp) es =
          if any (\e' -> happensBefore e e' eseq) es
          then []
          else [p]

-- Improve performance of this function
happensBefore :: Event -> Event -> Execution -> Bool
happensBefore (p,i,tp) (q,j,tq) eseq 
    | p==q = i < j
    | otherwise = 
        let pos = foldr (\i r -> M.insert i (elemIndices i $ fst3 $ unzip3 eseq) r) M.empty $ unique eseq
            pIndex = slookup p pos !! (i-1)
            qIndex = slookup q pos !! (j-1)
        in pIndex < qIndex && any dependent [(x,y) | x <- (V.toList tp), y <- (V.toList tq)]

dependent :: (Tag, Tag) -> Bool
dependent (Read x, Write y) = x==y 
dependent (Write x, Read y) = x==y 
dependent (Write x, Write y) = x==y 
dependent _ = False

checkIndep :: V.Vector Tag -> V.Vector Tag -> Bool
checkIndep tp tq = not $ any dependent [(x,y) | x <- (V.toList tp), y <- (V.toList tq)]

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

unique :: Execution -> [PID]
unique = nub . fst3 . unzip3

-- Lemma 6.1
-- minIndep :: Execution -> Events -> [Var]
(=~=) :: System -> State -> Execution -> [PID] -> [PID] -> Bool
(=~=) sys stEseq eseq []     w = True
(=~=) sys stEseq eseq (p:v') w = 
    let eW = toEvents sys stEseq eseq w
        iEw = minIndep eseq eW
        w'  = delete p w
        k   = getCount eseq p
        (stEseq', tp)  = step sys stEseq p
        eseq' = eseq ++ [(p,k+1,tp)]
        cond1 = p `elem` iEw && (=~=) sys stEseq' eseq' v' w' 
        cond2a = models sys stEseq p w 
        cond2b = (=~=) sys stEseq' eseq' v' w
        cond2 = cond2a && cond2b
    in cond1 || cond2

toEvents :: System -> State -> Execution -> [PID] -> Events
toEvents sys st eseq [] = []
toEvents sys st eseq (p:ps) = 
    let k = getCount eseq p
        (st',tp) = step sys st p
        e = (p,k+1,tp)
        eseq' = eseq ++ [e]
        es = toEvents sys st' eseq' ps
    in e:es  

-- E |= p <> w states that the next event of p would 
-- not "happen before" any event in w in the execution 
-- sequence E.p.w. Intuitively, it means that p is independent with w after E. 
class Models a where
    models :: System -> State -> Var -> a -> Bool 

instance Models Var where
--    models :: L.System -> L.State -> Var -> Var -> Bool
    models sys st p q = 
        let (st',tp) = step sys st p
            (_,tq)   = step sys st' q 
        in not $ any dependent [(x,y) | x <- (V.toList tp), y <- (V.toList tq)]

instance Models Event where
--    models :: L.System -> L.State -> Var -> Event -> Bool
    models sys st p (q,i,tq) = 
        let (st',tp) = step sys st p
            (_, tq)  = step sys st' q 
        in not $ any dependent [(x,y) | x <- (V.toList tp), y <- (V.toList tq)]
    
instance Models Events where
--    models :: L.System -> L.State -> Var -> Events -> Bool
    models sys st p [] = True         
    models sys st p es = 
        let (st',tp) = step sys st p
            (_,c) = foldr (checkStep tp) (st',True) es
        in c
        where 
          checkStep :: V.Vector Tag -> Event -> (State, Bool) -> (State, Bool)
          checkStep tp (q,_,_) (s, b) = 
                let (s',tq) = step sys s q
                in (s', b && checkIndep tp tq)
        
instance Models [PID] where 
    models sys st p [] = True
    models sys st p qs = 
        let (st',tp) = step sys st p
            (_,c) = foldr (checkStep' tp) (st',True) qs
        in c
        where 
          checkStep' :: V.Vector Tag -> PID -> (State, Bool) -> (State, Bool)
          checkStep' tp q (s, b) = 
                let (s',tq) = step sys s q
                in (s', b && checkIndep tp tq)
        
    
