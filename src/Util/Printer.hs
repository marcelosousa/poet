{-#LANGUAGE RecordWildCards, FlexibleInstances #-}
module Util.Printer where

import Control.Monad.ST.Safe
import qualified Data.HashTable.Class as H
import qualified Data.ByteString.Char8 as BS

import Exploration.UNF.APIStateless
import Model.GCS

import qualified Data.Map as M

unfToDot :: UnfolderState s -> ST s (Int,Int,String)
unfToDot sys@UnfolderState{..} = do
    events <- H.toList evts
    return (cntr, maxConf, "digraph unfolding {\n" ++ toDot events stack ++ "}")

class ToDot a where
    toDot :: a -> EventsID -> String
    
instance ToDot [(EventID, Event)] where
    toDot events stack = foldr (\el res -> toDot el stack ++ res) "" events

instance ToDot (EventID, Event) where
    toDot (eID, ev@Event{..}) stack = 
      let causality = foldr (printCausality eID) "" succ
          conflict = foldr (printConflict eID) "" icnf
          (pID,tID,act) = evtr
          label = show eID ++ " [label=\"eID = " ++ show eID ++ " tr=(" ++ show tID ++ "," ++ BS.unpack pID ++ "," ++ showActs act++")\"]\n"
      in causality ++ conflict ++ label

showActs :: Acts -> String
showActs acts = foldr (\act res -> showAct act ++ "," ++ res) "" acts

showAct :: Act -> String
showAct Other = "Other"
showAct (Lock (V var)) = "Lock " ++ BS.unpack var
showAct (Lock (A var idx)) = "Lock " ++ BS.unpack var ++ " " ++ show idx
showAct (Unlock (V var)) = "Unlock" ++ BS.unpack var
showAct (Unlock (A var idx)) = "Unlock" ++ BS.unpack var ++ " " ++ show idx


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