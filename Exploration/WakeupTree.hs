module Exploration.WakeupTree where

import Model.Language
import Exploration.Auxiliary
import Data.List

type WakeupTreeElem = [PID]
type WakeupTree = [WakeupTreeElem]

isEmpty :: WakeupTree -> Bool
isEmpty [] = True

min_prec :: WakeupTreeElem -> PID
min_prec = head
{-
min_prec :: WakeupTreeElem -> WakeupTree -> WakeupTreeElem
min_prec p [] = error "cant find this element"
min_prec p (x:xs) = 
    if p `isPrefixOf` x
    then x
    else min_prec p xs
-}

insertWuT :: System -> State -> Execution -> [PID] -> [PID] -> WakeupTree -> WakeupTree
insertWuT sys stE eseq w' w wut = 
    case foldr (\v' r -> if ((=~=) sys stE eseq v' w) then v':r else r) [] wut of
        []  -> w:wut
        vs' -> 
          let v = minimum vs'
          in if v `elem` wut
             then wut                            -- it is a Leaf
             else case anyCombination stE w v w' of 
                    Nothing -> w:wut             -- there is no equivalent
                    Just sw' -> (v ++ sw'):wut   -- there is an equivalent    

anyCombination :: State -> [PID] -> [PID] -> [PID] -> Maybe [PID]
anyCombination stE w v w' = 
    let allCombinations = [ permutations x | x <- nub $ subsequences w' ]
        allSortedCombinations = sortBy shortest $ nub $ concat allCombinations         
    in anyCombination' w v allSortedCombinations
    where
        anyCombination' w v [] = Nothing
        anyCombination' w v (w':ws') = 
            if subEquiv stE w (v ++ w')
            then Just w'
            else anyCombination' w v ws'

-- We are doing equality on state
subEquiv :: State -> [PID] -> [PID] -> Bool
subEquiv stE v w = 
    let v' = w \\ v
        allCombinations = [ permutations x | x <- nub $ subsequences v' ]
        allSortedCombinations = sortBy shortest $ nub $ concat allCombinations
    in subEquiv' stE v w allSortedCombinations
    where
        subEquiv' stE v w [] = False
        subEquiv' stE v w (v':r) = 
            let stVV' = execState stE $ v ++ v'
                stW   = execState stE w
            in stVV' == stW

shortest :: [PID] -> [PID] -> Ordering
shortest l1 l2 
 | length l1 < length l2 = LT
 | length l1 == length l2 = EQ
 | length l1 > length l2 = GT
 
pruneWuT :: PID -> WakeupTree -> WakeupTree
pruneWuT p = delete [] . nub . map (takeWhile (/=p))

subtree :: PID -> WakeupTree -> WakeupTree
subtree p = delete [] . nub . map (tail . dropWhile (/=p))


