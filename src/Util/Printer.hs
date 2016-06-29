{-#LANGUAGE RecordWildCards, FlexibleInstances #-}
module Util.Printer where

import Control.Monad.ST
import qualified Data.HashTable.Class as H
import qualified Data.ByteString.Char8 as BS

import Exploration.UNF.APIStateless
import Model.GCS

import qualified Data.Map as M

unfToDot :: Show act => UnfolderState st act s -> ST s (Int,Int,Int,String)
unfToDot sys@UnfolderState{..} = do
    events <- H.toList evts
    let maxConf = nr_max_conf stats
        cutoffCntr = nr_cutoffs stats
    return (cntr, maxConf, cutoffCntr, "digraph unfolding {\n" ++ toDot events stak ++ "}")

class ToDot a where
    toDot :: a -> EventsID -> String
    
instance Show act => ToDot [(EventID, Event act)] where
    toDot events stack = foldr (\el res -> toDot el stack ++ res) "" events

instance Show act => ToDot (EventID, Event act) where
    toDot (eID, ev@Event{..}) stack = 
      let causality = foldr (printCausality eID) "" succ
          conflict = foldr (printConflict eID) "" icnf
          (tID,pos) = name 
          label = show eID ++ " [label=\"eID = " ++ show eID 
               ++ " tr=(" ++ show tID ++ "," ++ show pos ++ "," ++ show acts++")\"]\n"
      in causality ++ conflict ++ label

showActs :: Show act => [act] -> String
showActs acts = foldr (\act res -> show act ++ "," ++ res) "" acts

printCausality :: EventID -> EventID -> String -> String
printCausality e1 e2 s = show e1 ++ " -> " ++ show e2 ++ ";\n" ++ s

printConflict :: EventID -> EventID -> String -> String
printConflict e1 e2 s = show e1 ++ " -> " ++ show e2 ++ " [style=dotted];\n" ++ s  

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
