module Frontend.PassTwo (passTwo) where

import Language.SimpleC.AST
import Language.C hiding (Ident)

import Frontend.Util

-- PASS 2
passTwo :: Globals -> Program -> Int
passTwo globals (Program (decl,defs)) =
    sum $ map assertWellFormed_1Global' defs
    where 
        assertWellFormed_1Global' (FunctionDef _ _ _ body) = 
            sum $ map (assertWellFormed_1Global globals) body
    
--
assertWellFormed_1Global :: Globals -> AnnStatement PC -> Int
assertWellFormed_1Global globals s = 
    case s of
        ExprStat _ e -> assertWellFormed_1GlobalE globals e
        Local _ _ Nothing -> 0
        Local _ _ (Just rhs) -> assertWellFormed_1GlobalE globals rhs
        IfThen _ cond _then -> 
            assertWellFormed_1GlobalE globals cond + sum (map (assertWellFormed_1Global globals) _then)
        If _ cond _then _else ->
               assertWellFormed_1GlobalE globals cond 
            + sum (map (assertWellFormed_1Global globals) _then)
            + sum (map (assertWellFormed_1Global globals) _else)
        While _ cond body ->
              assertWellFormed_1GlobalE globals cond 
           + sum (map (assertWellFormed_1Global globals) body)
        For _ init cond inc body ->
              assertWellFormed_1GlobalE globals init 
           + assertWellFormed_1GlobalE globals cond
           + assertWellFormed_1GlobalE globals inc
           + sum (map (assertWellFormed_1Global globals) body)
        Return _ _ -> error "assertWellFormed_1Global: return is disallowed!"
        Label _ _ s -> sum (map (assertWellFormed_1Global globals) s)
        Goto _ _ -> 0
        _ -> error $ "assertWellFormed: " ++ show s


allowedBinOp :: OpCode -> Bool
allowedBinOp op = 
 op `elem` [ CMulOp, CDivOp,CRmdOp, CAddOp, CSubOp, CLeOp,CGrOp,CLeqOp, CGeqOp,CEqOp,CNeqOp,CLndOp, CLorOp ]
    
assertWellFormed_1GlobalE :: Globals -> Expression -> Int
assertWellFormed_1GlobalE globals e =
    case e of
        BinOp op lhs rhs -> 
         if not (allowedBinOp op) 
         then error $ "assertWellFormed_1GlobalE: disallowed bin operator: " ++ show e
         else let lhsr = assertWellFormed_1GlobalE globals lhs 
                  rhsr = assertWellFormed_1GlobalE globals rhs
                  r = lhsr + rhsr
              in if  r > 1
                 then error $ "assertWellFormed_1GlobalE: more than one global " ++ show e
                 else r
        UnaryOp op expr ->
            case op of
                CPlusOp -> assertWellFormed_1GlobalE globals expr
                CMinOp  -> assertWellFormed_1GlobalE globals expr
                CNegOp  -> assertWellFormed_1GlobalE globals expr
                _ -> error $ "assertWellFormed_1GlobalE: disallowed unary op: " ++ show e
        Const v -> 0
        Ident x -> 
         if x `elem` globals
         then 1
         else 0
        Index lhs@(Ident _) rhs -> 
          let lhsr = assertWellFormed_1GlobalE globals lhs 
              rhsr = assertWellFormed_1GlobalE globals rhs
              r = lhsr + rhsr
          in if r > 1
             then error $ "assertWellFormed_1GlobalE: more than one global " ++ show e
             else if isIdentOrConstant rhs
                  then r
                  else error $ "Index operation only with ident or constant: " ++ show e
        Index _ _ -> error $ "assertWellFormed_1GlobalE: lhs of Index is not an identifier: " ++ show e
        Assign CAssignOp lhs rhs ->
          let lhsr = assertWellFormed_1GlobalE globals lhs 
              rhsr = assertWellFormed_1GlobalE globals rhs
              r = lhsr + rhsr
          in if r > 1
             then error $ "assertWellFormed_1GlobalE: more than one global " ++ show e
             else r
        Assign _ _ _ -> error $ "assertWellFormed_1GlobalE: disallowed assignment with operator " ++ show e
        Call ident _ -> case ident of
            "__poet_mutex_lock" -> 0
            "__poet_mutex_unlock" -> 0
            _ -> error $ "assertWellFormed_1GlobalE: function calls are disallowed " ++ show e
        _ -> error $ "assertWellFormed_1GlobalE: disallowed expression " ++ show e

isIdentOrConstant :: Expression -> Bool
isIdentOrConstant e = 
    case e of
      Const _ -> True
      Ident _ -> True
      _ -> False
