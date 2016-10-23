{-#LANGUAGE RecordWildCards, FlexibleInstances #-}
module Util.Printer where

import Control.Monad.ST
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.IO as HIO
import qualified Data.ByteString.Char8 as BS

--import qualified Exploration.UNF.APIStateless as O
import Exploration.SUNF.State 
import Model.GCS
import Haskroid.Hapiroid

import qualified Data.Map as M
{-
unfToDot :: Show act => O.UnfolderState st act s -> ST s (Int,Int,Int,String)
unfToDot sys@UnfolderState{..} = do
    events <- H.toList evts
    let maxConf = nr_max_conf stats
        cutoffCntr = nr_cutoffs stats
    return (cntr, maxConf, cutoffCntr, "digraph unfolding {\n" ++ toDot events stak ++ "}")
-}
unfToDot :: UnfolderState -> IO (Int,Int,Int,String)
unfToDot sys@UnfolderState{..} = do
    events <- HIO.toList evts
    let maxConf = nr_max_conf stats
        cutoffCntr = nr_cutoffs stats
    return (cntr, maxConf, cutoffCntr, "digraph unfolding {\n" ++ "node    [shape=box style=filled fillcolor=grey80];\n" ++ (fst $ toDot events stak) ++ "}")

-- class ToDot a where
--    toDot :: a -> EventsID -> String
    
-- instance ToDot [(EventID, Event)] where
toDot events stack = foldr (\el res -> toDotE el stack res) ("",[]) events

--instance ToDot (EventID, Event) where
toDotE (eID, ev@Event{..}) stack (s,l) = 
   let causality = foldr (printCausality eID) "" succ
       (conflict,l') = foldr (printConflict eID) ("",l) icnf
       (tID,pos) = name 
       label = show eID ++ " [label=\"" ++ "p:" ++ show tID ++ " " ++ show_pretty acts++"\"]\n"
   in (causality ++ conflict ++ label ++ s, l')

-- showActs :: Show act => [act] -> String
-- showActs acts = foldr (\act res -> show_pretty act ++ "," ++ res) "" acts

printCausality :: EventID -> EventID -> String -> String
printCausality e1 e2 s = show e1 ++ " -> " ++ show e2 ++ ";\n" ++ s

printConflict :: EventID -> EventID -> (String,[(EventID,EventID)]) -> (String,[(EventID,EventID)])
printConflict e1 e2 (s,l) = 
  if (e2,e1) `elem` l
  then (s,l) 
  else
   let ns = show e1 ++ " -> " ++ show e2  ++ " [style=dashed arrowhead=none color=red];\n" ++ s
   in (ns, (e1,e2):l) 

{-
printUnfConf :: UnfoldingPrefix -> String
printUnfConf (pes, cfs) = "digraph unfolding {\n" ++ printPES [] pes ++ "}" ++  printConfigurations cfs

printPES :: EventsID -> PES -> String
printPES mevents (events, causality, conflict) = 
 let events' = M.foldWithKey organizeEvents M.empty events  
 in M.foldWithKey (printEventRank mevents) "" events'  
 ++ foldr printCausality "" causality   
 ++ "subgraph Clf {\n" 
 ++ "  edge [dir=none]\n"
 ++ foldr printConflict  "" conflict  
 ++ "}"

organizeEvents :: EventID -> Event -> M.Map Int (M.Map EventID Event) -> M.Map Int (M.Map EventID Event)
organizeEvents id (tid,cid) m = 
     M.alter (\r -> case r of 
        Nothing -> Just $ M.singleton id (tid,cid)
        Just k  -> Just $ M.insert id (tid, cid) k) cid m 

printEventRank :: EventsID -> Int -> M.Map EventID Event -> String -> String
printEventRank mevents cid events s = 
  let s' = M.foldWithKey (printEvent mevents) "" events
  in "subgraph cluster"++show cid++" {\n"
  ++ "rank=same;\n"
  ++ s' 
  ++ "\n}\n"
    
printEvent :: EventsID -> EventID -> Event -> String -> String
printEvent mevents id (tid, cid) s = 
  if id `elem` mevents
  then show id ++ " [label=\""++tid++","++show cid++"\",style=filled,fillcolor=red];\n" ++ s
  else show id ++ " [label=\""++tid++","++show cid++"\"];\n" ++ s

printConfigurations :: Configurations -> String
printConfigurations = M.foldWithKey printConfiguration "" 

printConfiguration :: ConfigurationID -> (EventsID, Sigma) -> String -> String
printConfiguration cID (esID, s) r = show cID ++ "=" ++ show esID ++ ";" ++ show s ++ "\n" ++ r
-}
