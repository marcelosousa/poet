module Frontend where

import Language.SimpleC.AST
import Language.SimpleC.AST
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.C hiding (Ident)
import Language.C.System.GCC  -- preprocessor used

import Data.List
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
        pmd = GlobalDecl 0 (Ident "__poet_mutex_death") (Just (IntValue 0))
        threadNames = fst $ unzip threads
        defs' = replaceMain main' defs
        -- introduce a call to pthread_exit
        defs'' = map (introducePthreadCalls threadNames) defs'
        -- replace pthread_exit with mutex_unlock(__poet_mutex_threads[i]) + mutex_lock(__poet_mutex_death)
        defs''' = map (replacePthreadExit ("main":threadNames)) defs''
    in (Program (pmt:pmd:decl,defs'''),(threadCount,threads))

-- pass 3: rename variable x to "c.x"
alphaRename :: Globals -> Program -> Program
alphaRename  globals (Program (decls,defs)) = 
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
           
-- pass 6: fixing the PCs
--         getting rid of labels
--         transform local into exprStat of assign
--         confirm that at the top level we have only assign, goto, if, if-then-else.

-- pass 7: transform the program into a graph (data type to be defined)

replaceMain :: Definition -> [Definition] -> [Definition]
replaceMain nmain defs = 
    let others = filter (not . isMain) defs
    in nmain:others

introducePthreadCalls :: [Ident] -> Definition -> Definition
introducePthreadCalls names f@(FunctionDef pc "main" params stats) = 
    let mcall = ExprStat pc $ Call "pthread_exit" [Ident "NULL"]
    in FunctionDef pc "main" params $ stats++[mcall]
introducePthreadCalls names f@(FunctionDef pc name params stats) =     
    let mcall = ExprStat pc $ Call "pthread_exit" [Ident "NULL"]
        pcall = ExprStat pc (Call "__poet_mutex_lock" [Index (Ident "__poet_mutex_threads") (Ident name)])
    in if name `elem` names 
       then FunctionDef pc name params $ pcall:stats++[mcall]
       else f

replacePthreadExit :: [Ident] -> Definition -> Definition
replacePthreadExit names f@(FunctionDef pc name params stats) = 
    if name `elem` names
    then let stats' = foldr (\s r -> replacePthreadExitAux name s ++ r) [] stats
         in FunctionDef pc name params stats'
    else f

replacePthreadExitAux :: Ident -> AnnStatement PC -> [AnnStatement PC]
replacePthreadExitAux "main" s =
    case s of
         ExprStat pc (Call "pthread_exit" [Ident "NULL"]) ->
           let dcall = ExprStat pc (Call "__poet_mutex_lock" [Ident "__poet_mutex_death"])
           in [dcall]
         _ -> [s]
replacePthreadExitAux name s =
    case s of
         ExprStat pc (Call "pthread_exit" [Ident "NULL"]) ->
           let pcall = ExprStat pc (Call "__poet_mutex_lock" [Index (Ident "__poet_mutex_threads") (Ident name)])
               dcall = ExprStat pc (Call "__poet_mutex_lock" [Ident "__poet_mutex_death"])
           in [pcall,dcall]
         _ -> [s]

findAndInstrThreads :: Definition -> (Int,[(Ident,Int)], Definition)
findAndInstrThreads (FunctionDef pc "main" params stats) = 
    let (threadCount, threads, stats') = foldr (\s (i,r,r') -> let (s',t,i') = findAndInstrThreadsAux s i in (i',t++r,s'++r')) (0,[],[]) stats
        uniqueThreadNames = nub $ fst $ unzip threads
    in if length threads == length uniqueThreadNames
       then (threadCount, threads, FunctionDef pc "main" params stats')
       else error "thread names are not unique"

findAndInstrThreadsAux :: AnnStatement PC -> Int -> ([AnnStatement PC], [(Ident,Int)],Int)
findAndInstrThreadsAux s threadCount = 
    case s of
        ExprStat pc (Call "pthread_create" [Ident tid, Ident "NULL", Ident name, Ident "NULL"]) ->
          let i = Const $ IntValue $ toInteger threadCount
              ass = ExprStat pc (Assign CAssignOp (Ident tid) i)
              mcall = ExprStat pc (Call "__poet_mutex_lock" [Index (Ident "__poet_mutex_threads") i])
          in ([ass,mcall], [(name ,threadCount)], threadCount+1)
        ExprStat pc (Call "pthread_join" [Ident name, Ident "NULL"]) -> 
          let mcall = ExprStat pc (Call "__poet_mutex_lock" [Index (Ident "__poet_mutex_threads") (Ident name)])
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
              globals = getGlobalsDecls prog'
              res = assertWellFormed globals prog'
              prog'' = alphaRename globals prog'
          --print ctu
          print threads
          print prog
          print prog'
          print prog''
--          print res
          --print $ simplify $ translate ctu
