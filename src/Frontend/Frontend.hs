module Frontend where

import Language.SimpleC.AST
import Language.SimpleC.AST
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.C hiding (Ident)
import Language.C.System.GCC  -- preprocessor used

import Debug.Trace

type LabelID = Int
type DataBase = LabelID
type Globals = [Ident]

iDB :: DataBase
iDB = 0

isMain :: Definition -> Bool
isMain (FunctionDef _ "main" _ _) = True
isMain _ = False

findMain :: Defs -> Definition
findMain defs = case filter isMain defs of
    [main] -> main
    _ -> error "main not found"

pass1 :: Program -> (Program, (Int,[(Ident,Int)]))
pass1 p@(Program (decl,defs)) = 
    let main = findMain defs
        (threadCount, threads, main') = findAndInstrThreads main
        pmt = GlobalDecl 0 (Index (Ident "__poet_mutex_threads") (Const $ IntValue $ toInteger threadCount)) Nothing
    in (Program (pmt:decl,replaceMain main' defs),(threadCount,threads))

replaceMain :: Definition -> [Definition] -> [Definition]
replaceMain nmain defs = 
    let others = filter (not . isMain) defs
    in nmain:others
    
findAndInstrThreads :: Definition -> (Int,[(Ident,Int)], Definition)
findAndInstrThreads (FunctionDef pc "main" params stats) = 
    let (threadCount, threads, stats') = foldr (\s (i,r,r') -> let (s',t,i') = findAndInstrThreadsAux s i in (i',t++r,s'++r')) (0,[],[]) stats
    in (threadCount, threads, FunctionDef pc "main" params stats')

findAndInstrThreadsAux :: AnnStatement PC -> Int -> ([AnnStatement PC], [(Ident,Int)],Int)
findAndInstrThreadsAux s threadCount = 
    case s of
        ExprStat pc (Call "pthread_create" [Ident tid, Ident "NULL", Ident name, Ident "NULL"]) ->
          let i = Const $ IntValue $ toInteger threadCount
              ass = ExprStat pc (Assign CAssignOp (Ident tid) i)
              mcall = ExprStat pc (Call "__poet_mutex_lock" [Index (Ident "__poet_mutex_threads") i])
          in ([ass,mcall], [(name ,threadCount)], threadCount+1)
        ExprStat pc (Call "pthread_join" [Ident tid, Ident "NULL"]) -> 
          let mcall = ExprStat pc (Call "__poet_mutex_lock" [Index (Ident "__poet_mutex_threads") (Ident tid)])
          in ([mcall], [], threadCount)
        ExprStat _ (Call fname _) ->
          if fname `elem` ["pthread_create", "pthread_join"]
          then error "unexpected parameters for pthread_{create,join}"
          else ([s],[],threadCount) 
        _ -> ([s],[],threadCount)                     
        
simplify :: Program -> Program
simplify (Program (decl,defs)) = 
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

-- PASS 2
assertWellFormed :: Globals -> Program -> Int
assertWellFormed globals (Program (decl,defs)) =
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

getGlobalsDecls :: Program -> Globals
getGlobalsDecls (Program (decls,defs)) = foldl (\a decl -> convertDecl decl ++ a) [] decls
  where 
    convertDecl decl = case decl of
      FunctionDecl _ _ _ -> [] 
      GlobalDecl _ (Ident i) _ -> [i]
      GlobalDecl _ (Index (Ident i) _) _ -> [i]
      GlobalDecl _ _ _ -> []

-- 


parseFile :: FilePath -> IO CTranslUnit
parseFile f  =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] f
     case parse_result of
       Left parse_err -> do 
           parse_result <- parseCFilePre f
           case parse_result of
               Left _ -> error (show parse_err)
               Right ast -> return ast
       Right ast      -> return ast

pp :: FilePath -> IO ()
pp f = do ctu <- parseFile f
          let prog = translate ctu
              (prog',threads) = pass1 prog
              globals = getGlobalsDecls prog
              res = assertWellFormed globals prog
          --print ctu
          print threads
          print prog
          print prog'
--          print res
          --print $ simplify $ translate ctu
