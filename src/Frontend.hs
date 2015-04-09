module Frontend where

import Language.SimpleC.AST
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.C hiding (Ident)
import Language.C.System.GCC  -- preprocessor used

import Data.List
import Debug.Trace
import qualified Data.Map as M
import Data.Map (Map)

import Frontend.Util
import Frontend.PassOne
import Frontend.PassTwo
import Frontend.PassThree
import Frontend.PassFourFive
import Frontend.PassSix
import Frontend.PassSeven

-- frontEnd: 
--  Transforms a C file into a restricted subset of C
--  for easy manipulation and analysis.
frontEnd :: Program -> (Program, FirstFlow, Flow, Int)
frontEnd prog = 
    let globals = getGlobalsDecls prog
        (prog1, threadCount) = passOne prog
        pass2res = passTwo globals prog1 -- :: Int
        prog3 = passThree globals prog1
        prog45 = passFourFive prog3
        prog6 = passSix prog45
        pass7res = pass2res `seq` passSeven prog6 -- :: Bool
        maplabelpc = mapLabelPC prog6
        prog8 = removeLabel prog6
        (firstflow, flow) = flowProgram maplabelpc prog8
    in if pass7res
       then (prog8, firstflow, flow, threadCount)
       else error "frontEnd fatal: something went wrong! Please contact developers."

-- Pass 8: transform the program into a graph (data type to be defined)
-- Pass 8a: get the pc for all the labels; 
--          remove the labels;
--          goto to the pc of the label;
type MapLabelPC = Map Ident PC

mapLabelPC :: Program -> MapLabelPC
mapLabelPC (Program (decls,defs)) = 
    let rawmap = concatMap mapLabelPCDef defs 
        idents = length $ nub $ fst $ unzip rawmap
    in if length rawmap == idents
       then M.fromList rawmap
       else error "mapLabelPC: two labels with the same ident were defined"
  where
      mapLabelPCDef (FunctionDef _ _ _ ss) = concatMap mapLabelPCStat ss
      mapLabelPCStat s = case s of
          IfThen _ _ _then -> concatMap mapLabelPCStat _then
          If _ _ _then _else -> 
            let mlpct = concatMap mapLabelPCStat _then
                mlpce = concatMap mapLabelPCStat _else
            in mlpct ++ mlpce
          Label pc i body -> (i,pc):concatMap mapLabelPCStat body
          _ -> []

removeLabel :: Program -> Program
removeLabel (Program (decls,defs)) = 
    let defs' = map removeLabelDef defs
    in Program (decls, defs')
  where
      removeLabelDef (FunctionDef pc i p ss) = FunctionDef pc i p $ concatMap removeLabelStat ss
      removeLabelStat s = case s of
          ExprStat pc e -> [ExprStat pc e]
          IfThen pc c _then -> [IfThen pc c $ concatMap removeLabelStat _then]
          If pc cond _then _else -> 
            let _then' = concatMap removeLabelStat _then
                _else' = concatMap removeLabelStat _else
            in [If pc cond _then' _else']
          Label pc i body -> concatMap removeLabelStat body
          Goto pc i -> [Goto pc i]
          _ -> error $ "getPC fatal: disallowed " ++ show s
          
data Dir = Branch (PC, PC) | Continue PC
  deriving Show
type Flow = [(PC,Dir)]
type FirstFlow = [(Ident,PC)]

getFlow :: Flow -> PC -> Dir
getFlow [] pc = error $ "getFlow: no match for " ++ show pc
getFlow ((pc',dir):rest) pc = 
    if pc == pc'
    then dir
    else getFlow rest pc
    
getPC :: AnnStatement PC -> PC
getPC s = case s of
    ExprStat pc e -> pc
    IfThen pc _ _then -> pc
    If pc cond _then _else -> pc
    Goto pc i -> pc
    _ -> error $ "getPC fatal: disallowed " ++ show s
  
flowProgram :: MapLabelPC -> Program -> (FirstFlow, Flow)
flowProgram mlpc (Program (decls,defs)) = 
    let flowdefs = map flowDefinition defs
        (firstpc, flowdefs') = unzip flowdefs
    in (firstpc, concat $ fst $ unzip flowdefs') where
        flowDefinition (FunctionDef pc ident params stats) = 
            ((ident, getPC (head stats)), flowStatement mlpc Nothing stats)
 
flowStatement :: MapLabelPC -> Maybe PC -> Statement -> (Flow, [PC])
flowStatement mlpc _ [] = error "flowStatement: empty list"
flowStatement mlpc nextPC [s] = case s of
    ExprStat pc e -> ([], [pc])
    IfThen pc _ _then -> 
      let nextTPC = getPC $ head _then
          nextPC' = if length _then > 1 then (Just $ getPC $ _then!!1) else nextPC
          this = case nextPC' of
              Nothing -> error $ "flowStatement: can't happen " ++ show s ++ " " ++ show nextPC
              Just _nextPC -> (pc, Branch (nextTPC, _nextPC))
          (thenFlow, etpc) = flowStatement mlpc nextPC' _then
      in (this:thenFlow, pc:etpc)
    If pc cond _then _else -> 
      let nextTPC = getPC $ head _then
          nextPCT = if length _then > 1 then (Just $ getPC $ _then!!1) else nextPC
          nextFPC = getPC $ head _else
          nextPCE = if length _else > 1 then (Just $ getPC $ _else!!1) else nextPC
          this = (pc, Branch (nextTPC, nextFPC))
          (thenFlow, etpc) = flowStatement mlpc nextPCT _then
          (elseFlow, eepc) = flowStatement mlpc nextPCE _else
      in (this:(thenFlow++elseFlow), etpc++eepc)
    Goto pc i ->
      case M.lookup i mlpc of
          Nothing -> error "flowStatement: goto nowhere!"
          Just lpc -> ([(pc, Continue lpc)], [lpc]) 
    _ -> error $ "flowStatement fatal: disallowed " ++ show s
flowStatement mlpc nextPC (s:n:ss) = case s of
    ExprStat pc e -> 
      let this = (pc, Continue $ getPC n)
          (next, epc) = flowStatement mlpc nextPC (n:ss)
      in (this:next, epc)
    IfThen pc _ _then -> 
      let nextFPC = getPC n 
          nextTPC = getPC $ head _then
          this = (pc, Branch (nextTPC, nextFPC))
          (thenFlow, etpc) = flowStatement mlpc (Just nextFPC) _then
          finalThen = map (\et -> (et, Continue nextFPC)) etpc
          (next, epc) = flowStatement mlpc Nothing (n:ss)
      in (this:(thenFlow++next), epc)
    If pc cond _then _else -> 
      let nextPC = getPC n 
          nextTPC = getPC $ head _then
          nextFPC = getPC $ head _else
          this = (pc, Branch (nextTPC, nextFPC))
          (thenFlow, etpc) = flowStatement mlpc (Just nextPC) _then
          finalThen = map (\et -> (et, Continue nextPC)) etpc
          (elseFlow, eepc) = flowStatement mlpc (Just nextPC) _else
          finalElse = map (\et -> (et, Continue nextPC)) eepc
          (next, epc) = flowStatement mlpc Nothing (n:ss)
      in (this:(thenFlow++elseFlow++next), epc)
    Goto pc i ->
      case M.lookup i mlpc of
          Nothing -> error "flowStatement: goto nowhere!"
          Just lpc -> ([(pc, Continue lpc)], [lpc]) 
    _ -> error $ "flowStatement fatal: disallowed " ++ show s