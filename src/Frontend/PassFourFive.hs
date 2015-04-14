module Frontend.PassFourFive (passFourFive) where

import Language.SimpleC.AST
import Debug.Trace

type LabelID = Int
type DataBase = LabelID

iDB :: DataBase
iDB = 0

-- PASS 4 AND 5
passFourFive :: Program -> Program
passFourFive (Program (decl,defs)) = 
    let defs' = fst $ unzip $ map (applyTrans iDB for2while) defs -- Pass 4
        defs'' = fst $ foldr (\def (rest,db) -> let (def',db') = applyTrans db while2if def in (def':rest,db')) ([],iDB) defs' -- Pass 5
    in Program (decl,defs'')

applyTrans :: DataBase -> (DataBase -> AnnStatement PC -> ([AnnStatement PC],DataBase)) -> Definition -> (Definition,DataBase)
applyTrans db f (FunctionDef pc c p body) = 
    let (db',body') = foldr (\s (db',r) -> let (s',db'') = f db' s in (db'', s' ++ r)) (db,[]) body
    in (FunctionDef pc c p body',db')
    
for2while :: DataBase -> AnnStatement PC -> ([AnnStatement PC],DataBase)
for2while db s = case s of
    For pc ini cond incr body ->         
        let (_body,db') = foldr (\def (rest,db) -> let (def',db') = for2while db def in (def'++rest,db')) ([],db) body
            body' = _body ++ [ExprStat pc incr]
        in ([ExprStat pc ini, While pc cond body'],db')
    IfThen pc expr stat ->
        let (stat',db') = foldr (\def (rest,db) -> let (def',db') = for2while db def in (def'++rest,db')) ([],db) stat
        in ([IfThen pc expr stat'],db')
    If pc expr _then _else ->
        let (_then',db') = foldr (\def (rest,db) -> let (def',db') =  for2while db def in (def'++rest,db')) ([],db) _then
            (_else',db'') = foldr (\def (rest,db) -> let (def',db') =  for2while db def in (def'++rest,db')) ([],db') _else
        in ([If pc expr _then' _else'],db'')
    While pc cond body ->
        let (_body,db') = foldr (\def (rest,db) -> let (def',db') = for2while db def in (def'++rest,db')) ([],db) body
        in ([While pc cond _body],db')
    Label pc i body ->
        let (_body,db') = foldr (\def (rest,db) -> let (def',db') = for2while db def in (def'++rest,db')) ([],db) body
        in ([Label pc i _body],db')
    _ -> ([s],db)

freshLabel :: DataBase -> (DataBase, Ident)
freshLabel d = (d+1, show d)

while2if ::  DataBase -> AnnStatement PC -> ([AnnStatement PC],DataBase)
while2if db s = case s of
    While pc cond body -> 
        let (db',label) = freshLabel db
            (_body,db'') = foldr (\def (rest,db) -> let (def',db') = while2if db def in (def'++rest,db')) ([],db') body
            body' = _body ++ [Goto pc label]
            ifS = IfThen pc cond body'
            labelS = Label pc label [ifS]                
        in ([labelS],db'')
    IfThen pc expr stat ->
        let (stat',db') = foldr (\def (rest,db) -> let (def',db') = while2if db def in (def'++rest,db')) ([],db) stat
        in ([IfThen pc expr stat'],db')
    If pc expr _then _else ->
        let (_then',db') = foldr (\def (rest,db) -> let (def',db') =  while2if db def in (def'++rest,db')) ([],db) _then
            (_else',db'') = foldr (\def (rest,db) -> let (def',db') =  while2if db def in (def'++rest,db')) ([],db') _else
        in ([If pc expr _then' _else'],db'')
    Label pc i body ->
        let (_body,db') = foldr (\def (rest,db) -> let (def',db') = while2if db def in (def'++rest,db')) ([],db) body
        in ([Label pc i _body],db')
    For _ _ _ _ _ -> error "while2if"
    _ -> ([s],db)
