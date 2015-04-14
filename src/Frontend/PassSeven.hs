module Frontend.PassSeven (passSeven) where

import Language.SimpleC.AST
import Language.SimpleC.Printer

-- PASS 7
-- confirm that at the top level we have only assign, goto, if, if-then-else and labels.
passSeven :: Program -> Bool
passSeven (Program (decls,defs)) = 
    all (\(FunctionDef _ _ _ stats) -> all withinRestrictions stats) defs

poetFuncNames :: [String]
poetFuncNames = 
    ["__poet_mutex_lock", "__poet_mutex_unlock", "__poet_fail"]
    
withinRestrictions :: AnnStatement PC -> Bool
withinRestrictions s =
    case s of
        ExprStat _ e -> 
          case e of 
            Assign _ _ _ -> True
            Call name _ ->
              if name `elem` poetFuncNames
              then True
              else error $ "sanityCheck: " ++ show e
        IfThen _ _ _then -> 
          all withinRestrictions _then
        If pc cond _then _else -> 
          let _thenR = all withinRestrictions _then
              _elseR = all withinRestrictions _else
          in _thenR && _elseR
        Label _ i body -> 
          all withinRestrictions body
        Goto _ i -> True 
        _ -> error $ "sanityCheck: disallowed " ++ show s
