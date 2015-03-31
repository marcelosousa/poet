module Frontend.PassThree (passThree) where

import Language.SimpleC.AST

import Frontend.Util

-- PASS 3
-- rename variable x to "c.x"
passThree :: Globals -> Program -> Program
passThree globals (Program (decls,defs)) = 
    let defs' = map (varRename globals) defs
    in Program (decls, defs')
    
varRename :: Globals -> Definition -> Definition
varRename globals (FunctionDef pc name params stats) = 
    let stats' = map (varRenameAux name globals) stats
    in FunctionDef pc name params stats'

varRenameAux :: Ident -> Globals -> AnnStatement PC -> AnnStatement PC
varRenameAux name globals s =
    case s of
        ExprStat pc e -> ExprStat pc $ varRenameAux' name globals e
        Local pc e me -> Local pc (varRenameAux' name globals e) $ fmap (varRenameAux' name globals) me
        IfThen pc cond _then -> 
          let cond' = varRenameAux' name globals cond
              _then' = map (varRenameAux name globals) _then
          in IfThen pc cond' _then'
        If pc cond _then _else ->
            let cond' = varRenameAux' name globals cond
                _then' = map (varRenameAux name globals) _then
                _else' = map (varRenameAux name globals) _else
            in If pc cond' _then' _else' 
        While pc cond body ->
            let cond' = varRenameAux' name globals cond
                body' = map (varRenameAux name globals) body
            in While pc cond' body'
        For pc init cond inc body ->
            let init' = varRenameAux' name globals init
                cond' = varRenameAux' name globals cond
                inc' = varRenameAux' name globals inc
                body' = map (varRenameAux name globals) body
            in For pc init' cond' inc' body'
        Return _ _  -> error "varRenameAux: return is disallowed!"
        _ -> s

varRenameAux' :: Ident -> Globals -> Expression -> Expression
varRenameAux' name globals e =
    case e of
        BinOp op lhs rhs ->
          let lhs' = varRenameAux' name globals lhs
              rhs' = varRenameAux' name globals rhs
          in BinOp op lhs' rhs'
        UnaryOp op expr -> UnaryOp op $ varRenameAux' name globals expr
        Ident i -> Ident $ varRenameIdent name globals i 
        Index lhs rhs ->
          let lhs' = varRenameAux' name globals lhs
              rhs' = varRenameAux' name globals rhs
          in Index lhs' rhs'
        Assign op lhs rhs ->
          let lhs' = varRenameAux' name globals lhs
              rhs' = varRenameAux' name globals rhs
          in Assign op lhs' rhs'
        _ -> e
        
varRenameIdent :: Ident -> Globals -> Ident -> Ident
varRenameIdent name globals i =
    if i `elem` globals
    then i
    else name++"."++i
