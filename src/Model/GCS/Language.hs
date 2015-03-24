{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module Model.Language where

import qualified Data.Map as M
import qualified Data.Vector as V
import Debug.Trace

-- Safe Lookup on a Map
slookup :: (Ord k, Show k) => k -> M.Map k a -> a
slookup k m = case M.lookup k m of
    Nothing -> error $ "var " ++ show k ++ " not in env"
    Just a  -> a

{-
-- Tag Type
-- Each transition yields a list 
-- of tags about the type of operations.
-- Later on this can be extended with 
-- the values.
-}
data Tag = Read Var 
         | Write Var
         | Neutral
  deriving (Show,Eq,Ord)

type Value = Int
type Var   = String
type PID   = Var

-- Sigma
-- We have a pair (Valuation, EnabledProcesses)
type Valuation = M.Map Var Value
type State = (Valuation, [Var])
type Execution = Events -- [(PID, V.Vector Tag)] -- [Event]

type Event = (Var, Int, V.Vector Tag)
type Events = [Event]
type Race = (Event, Event)
type Races = [Race]

toVar :: Events -> [PID]
toVar = map (\(p,_,_) -> p)

emptyState :: State
emptyState = (M.empty, [])

getCount :: Execution -> PID -> Int
getCount eseq p = 
    foldr (\(q,_,_) i -> if q==p then i+1 else i) 0 eseq

type Transition = State -> (State, V.Vector Tag)
type TransitionSystem = M.Map Var Transition
type PC = Int

--type Flow = 
--type PTransition = [Transition]
--type PCs = M.Map Var PC
--type TState = (PC, Valuation)
--type State = (PCs, Valuation)

data System = 
    System { initial :: State
           , trans   :: TransitionSystem }

enabled :: State -> Maybe PID
enabled (val, []) = Nothing
enabled (val, (p:ps)) = Just p

{-
enabled :: System -> State -> Maybe Transition
enabled sys (val, []) = Nothing
enabled sys (val, (p:ps)) = M.lookup p $ trans sys 
-}

class Fix a b | a -> b where
    fix :: a -> b -> b

step :: System -> State -> PID -> (State, V.Vector Tag)
step sys s pid = 
    let tr = slookup pid $ trans sys 
    in tr s
    
exec :: System -> State
exec (System i tr) = fix tr i

execState :: State -> [PID] -> State
execState = undefined

instance Fix TransitionSystem State where
    fix tr s@(v,[]) = s
    fix tr s@(v,(p:ps)) = 
        let trP = slookup p tr
            (s',_) = trP s
        in trace ("running with val " ++ show v) $ fix tr s'

{-        
fix :: Transition -> State -> State
fix tr s = 
    if deadlock s
    then s
    else let (s',_) = tr s
         in fix tr s'


deadlock :: State -> Bool


enabled (pcs,_) = enabled' $ M.toList pcs
 where enabled' :: [(Var,PC)] -> Maybe (Var,PC)
       enabled' [] = Nothing
       enabled' ((v,-1):xs) = enabled' xs
       enabled' ((v,pc):xs) = Just (v,pc)

allEnabled :: [(Var,PC)] -> [(Var,PC)]
allEnabled = filter (\(_,pc) -> pc /= (-1)) 
    

step :: PTransition -> State -> (Var,PC) -> State
step tr (pcs,e) (v,pc) =
     let t = slookup v tr
         (pc', e') = t (pc,e)
         pcs' = M.adjust (const pc') v pcs
     in (pcs',e')
     
dfsSimple :: System -> [State]
dfsSimple (System tr i) = dfs tr i []

dfs :: PTransition -> State -> [State] -> [State]
dfs tr s@(pcs,e) sts = 
    let sts' = s:sts
        trs  = allEnabled $ M.toList pcs
    in foldr (dfs' tr s) sts' trs

dfs' :: PTransition -> State -> (Var,PC) -> [State] -> [State]
dfs' tr s (v,pc) sts = 
    let s' = step tr s (v,pc)
    in if elem s' sts
       then sts
       else dfs tr s' sts

pretty :: [State] -> String
pretty = foldr prettyState ""

prettyState :: State -> String -> String
prettyState (pcs, e) r = "[" ++ prettyPCs (M.toList pcs) ++ "] - " ++ prettyVal (M.toList e) ++ "\n" ++ r

prettyPCs :: [(Var, PC)] -> String
prettyPCs [(v,p)] = v++"@"++(show p)
prettyPCs ((v,p):xs) = v++"@"++(show p) ++ ", " ++ prettyPCs xs

prettyVal :: [(Var, Value)] -> String
prettyVal [(v,p)] = v++"="++(show p)
prettyVal ((v,p):xs) = v++"="++(show p) ++ ", " ++ prettyVal xs

-}
