module Printer where

import Model
import APIStateful
import qualified Data.Map as M

printUnf :: EventsID -> UnfoldingPrefix -> String
printUnf mev unf@(pes, cfs) = "digraph unfolding {\n" ++ printPES mev pes ++ "}" 

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

printCausality :: (EventID, EventID) -> String -> String
printCausality (e1,e2) s = show e1 ++ " -> " ++ show e2 ++ ";\n" ++ s

printConflict :: (EventID, EventID) -> String -> String
printConflict (e1,e2) s = show e1 ++ " -> " ++ show e2 ++ " [style=dotted];\n" ++ s  

printConfigurations :: Configurations -> String
printConfigurations = M.foldWithKey printConfiguration "" 

printConfiguration :: ConfigurationID -> (EventsID, Sigma) -> String -> String
printConfiguration cID (esID, s) r = show cID ++ "=" ++ show esID ++ ";" ++ show s ++ "\n" ++ r
