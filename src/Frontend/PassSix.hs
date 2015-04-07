module Frontend.PassSix (passSix) where

import Language.SimpleC.AST
import Language.SimpleC.Printer
import Language.C hiding (Ident)

-- PASS 6
--  getting rid of labels (no)
passSix :: Program -> Program
passSix (Program (decls,defs)) = 
    -- transform local into exprStat of assign
    let defs' = map local2exprStat defs
    -- fix the PCs
        defs'' = fst $ foldr (\def (r,c) -> let (def',nc) = fixPCDef c def in (def':r,nc)) ([],0) defs'
    in Program (decls,defs'')

local2exprStat :: Definition -> Definition
local2exprStat (FunctionDef pc name params stats) = 
    FunctionDef pc name params $ map local2exprStat' stats

local2exprStat' :: AnnStatement PC -> AnnStatement PC
local2exprStat' s =
    case s of
        Local pc lhs Nothing -> ExprStat pc $ Assign CAssignOp lhs $ Const $ IntValue 0
        Local pc lhs (Just rhs) -> ExprStat pc $ Assign CAssignOp lhs rhs
        _ -> s

fixPCDef :: PC -> Definition -> (Definition, PC)
fixPCDef pcCount (FunctionDef _ name params stats) =
    let npcCount = pcCount + 1
        (stats', pcCount') = rec npcCount fixPCStat stats
    in (FunctionDef pcCount name params stats', pcCount')

fixPCStat :: PC -> AnnStatement PC -> (AnnStatement PC, PC)
fixPCStat pcCount s = 
    let pcCount' = pcCount + 1
    in case s of
        ExprStat _ e -> (ExprStat pcCount e, pcCount')
        IfThen _ cond _then -> 
          let (_then', npc) = rec pcCount' fixPCStat _then
          in (IfThen pcCount cond _then', npc)
        If pc cond _then _else -> 
          let (_then', npc) = rec pcCount' fixPCStat _then
              (_else', npc') = rec npc fixPCStat _else
          in (If pcCount cond _then' _else', npc')
        Label _ i body -> 
          let (body', npc) = rec pcCount' fixPCStat body
          in (Label pcCount i body', npc)
        Goto _ i -> 
          (Goto pcCount i, pcCount') 
        _ -> error $ "fixPCStat: disallowed " ++ show s

rec :: a -> (a -> b -> (b,a)) -> [b] -> ([b], a)
rec c func list = 
    foldr (\el (res,cc) -> let (el',nc) = func cc el in (el':res,nc)) ([],c) list
