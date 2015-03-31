module Frontend.PassOne (passOne) where

import Language.SimpleC.AST
import Language.C hiding (Ident)

import Data.List

-- PASS 1
--passOne :: Program -> (Program, (Int,[(Ident,Int)]))
passOne :: Program -> Program
passOne p@(Program (decl,defs)) = 
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
    in Program (pmt:pmd:decl,defs''')
--    in (Program (pmt:pmd:decl,defs'''),(threadCount,threads))

isMain :: Definition -> Bool
isMain (FunctionDef _ "main" _ _) = True
isMain _ = False

findMain :: Defs -> Definition
findMain defs = case filter isMain defs of
    [main] -> main
    _ -> error "main not found"

replaceMain :: Definition -> [Definition] -> [Definition]
replaceMain nmain defs = 
    let others = filter (not . isMain) defs
    in nmain:others

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
