module Frontend.PassFourFive (passFourFive) where

import Language.SimpleC.AST

type LabelID = Int
type DataBase = LabelID

iDB :: DataBase
iDB = 0

-- PASS 4 AND 5
passFourFive :: Program -> Program
passFourFive (Program (decl,defs)) = 
    let defs' = fst $ unzip $ map (applyTrans iDB for2while) defs -- Pass 4
        defs'' = fst $ unzip $ map (applyTrans iDB while2if) defs' -- Pass 5
    in Program (decl,defs'')

applyTrans :: DataBase -> (DataBase -> AnnStatement PC -> ([AnnStatement PC],DataBase)) -> Definition -> (Definition,DataBase)
applyTrans db f (FunctionDef pc c p body) = 
    let (db',body') = foldr (\s (db',r) -> let (s',db'') = f db' s in (db'', s' ++ r)) (db,[]) body
    in (FunctionDef pc c p body',db')
    
for2while :: DataBase -> AnnStatement PC -> ([AnnStatement PC],DataBase)
for2while db s = case s of
    For pc ini cond incr body -> 
        let body' = body ++ [ExprStat pc incr]
        in ([ExprStat pc ini, While pc cond body'],db)
    _ -> ([s],db)

freshLabel :: DataBase -> (DataBase, Ident)
freshLabel d = (d+1, show d)

while2if ::  DataBase -> AnnStatement PC -> ([AnnStatement PC],DataBase)
while2if db s = case s of
    While pc cond body -> 
        let (db',label) = freshLabel db
            body' = body ++ [Goto pc label]
            ifS = IfThen pc cond body'
            labelS = Label pc label [ifS]                
        in ([labelS],db')
    _ -> ([s],db)

