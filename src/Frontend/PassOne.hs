module Frontend.PassOne (passOne) where

import Language.SimpleC.AST
import Language.C hiding (Ident)

import Debug.Trace

import Data.List

-- PASS 1
--passOne :: Program -> (Program, (Int,[(Ident,Int)]))
passOne :: Program -> (Program, Int)
passOne p@(Program (decl,defs)) = 
    let main = findMain defs
        (threadCount, threads, main') = findAndInstrThreads main
        pmt = GlobalDecl 0 (Index (Ident "__poet_mutex_threads") (Const $ IntValue $ toInteger threadCount)) Nothing
        pmj = GlobalDecl 0 (Index (Ident "__poet_mutex_threads_join") (Const $ IntValue $ toInteger threadCount)) Nothing
        pmd = GlobalDecl 0 (Ident "__poet_mutex_death") (Just (IntValue 0))
        threadNames = fst3 $ unzip3 threads
        defs' = replaceMain main' defs
        -- introduce a call to pthread_exit
        defs'' = map (introducePthreadCalls threads) defs'
        -- replace pthread_exit with mutex_unlock(__poet_mutex_threads[i]) + mutex_lock(__poet_mutex_death)
        defs''' = map (replacePthreadExit ("main":threadNames) threads) defs''
    in (Program (pmt:pmd:decl,defs'''), threadCount)
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

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

findAndInstrThreads :: Definition -> (Int,[(Ident,Ident,Int)], Definition)
findAndInstrThreads (FunctionDef pc "main" params stats) = 
    let (threadCount, threads, stats') = foldl (\(i,r,r') s -> let (s',t,i') = findAndInstrThreadsAux s (r,i) in (i',t,r'++s')) (0,[],[]) stats
        uniqueThreadNames = nub $ fst3 $ unzip3 threads
    in if length threads == length uniqueThreadNames
       then (threadCount, threads, FunctionDef pc "main" params stats')
       else error "thread names are not unique"

findAndInstrThreadsAux :: AnnStatement PC -> ([(Ident,Ident,Int)],Int) -> ([AnnStatement PC], [(Ident,Ident,Int)],Int)
findAndInstrThreadsAux s (maps,threadCount) = 
    case s of
        ExprStat pc (Call "pthread_create" [Ident tid, Ident "NULL", Ident name, Ident "NULL"]) ->
          let i = Const $ IntValue $ toInteger threadCount
              --ass = ExprStat pc (Assign CAssignOp (Ident tid) i)
              mcall = ExprStat pc (Call "__poet_mutex_unlock" [Index (Ident "__poet_mutex_threads") i])
              res = (name,tid,threadCount):maps
          in ([mcall], res, threadCount+1)
        ExprStat pc (Call "pthread_join" [Ident tid, Ident "NULL"]) -> 
          let i = Const $ IntValue $ toInteger $ getTIDValue tid maps
              mcall = ExprStat pc (Call "__poet_mutex_lock" [Index (Ident "__poet_mutex_threads_join") i])
          in ([mcall], maps, threadCount)
        ExprStat _ (Call fname _) ->
          if fname `elem` ["pthread_create", "pthread_join"]
          then error "unexpected parameters for pthread_{create,join}"
          else ([s],maps,threadCount) 
        _ -> ([s],maps,threadCount)

getTIDValue :: Ident -> [(Ident,Ident,Int)] -> Int
getTIDValue s [] = error $ "getTIDValue: not found " ++ show s
getTIDValue s ((_,s',i):rest) =
    if s == s'
    then i
    else getTIDValue s rest

getTNameValue :: Ident -> [(Ident,Ident,Int)] -> Int
getTNameValue s [] = error "getTNameValue: not found"
getTNameValue s ((s',_,i):rest) =
    if s == s'
    then i
    else getTNameValue s rest
    
introducePthreadCalls :: [(Ident,Ident,Int)] -> Definition -> Definition
introducePthreadCalls names f@(FunctionDef pc "main" params stats) = 
    let mcall = ExprStat pc $ Call "pthread_exit" [Ident "NULL"]
    in FunctionDef pc "main" params $ stats++[mcall]
introducePthreadCalls names f@(FunctionDef pc name params stats) =   
    let mcall = ExprStat pc $ Call "pthread_exit" [Ident "NULL"]
        i = Const $ IntValue $ toInteger $ getTNameValue name names
        tnames = fst3 $ unzip3 $ names
        pcall = ExprStat pc (Call "__poet_mutex_lock" [Index (Ident "__poet_mutex_threads") i])
    in if name `elem` tnames
       then FunctionDef pc name params $ pcall:stats++[mcall]
       else f

replacePthreadExit :: [Ident] -> [(Ident,Ident,Int)] -> Definition -> Definition
replacePthreadExit names threadInfo f@(FunctionDef pc name params stats) = 
    if name `elem` names
    then let stats' = foldr (\s r -> replacePthreadExitAux name threadInfo s ++ r) [] stats
         in FunctionDef pc name params stats'
    else f

replacePthreadExitAux :: Ident -> [(Ident,Ident,Int)]  -> AnnStatement PC -> [AnnStatement PC]
replacePthreadExitAux "main" threadInfo s =
    case s of
         ExprStat pc (Call "pthread_exit" [Ident "NULL"]) ->
           let dcall = ExprStat pc (Call "__poet_mutex_lock" [Ident "__poet_mutex_death"])
           in [dcall]
         ExprStat pc (Call "pthread_mutex_lock" param) ->
           let dcall = ExprStat pc (Call "__poet_mutex_lock" param)
           in [dcall]
         ExprStat pc (Call "pthread_mutex_unlock" param) ->
           let dcall = ExprStat pc (Call "__poet_mutex_unlock" param)
           in [dcall]
         _ -> [s]
replacePthreadExitAux name threadInfo s =
    case s of
         ExprStat pc (Call "pthread_exit" [Ident "NULL"]) ->
           let i = Const $ IntValue $ toInteger $ getTNameValue name threadInfo
               pcall = ExprStat pc (Call "__poet_mutex_unlock" [Index (Ident "__poet_mutex_threads_join") i])
               dcall = ExprStat pc (Call "__poet_mutex_lock" [Ident "__poet_mutex_death"])
           in [pcall,dcall]
         ExprStat pc (Call "pthread_mutex_lock" param) ->
           let dcall = ExprStat pc (Call "__poet_mutex_lock" param)
           in [dcall]
         ExprStat pc (Call "pthread_mutex_unlock" param) ->
           let dcall = ExprStat pc (Call "__poet_mutex_unlock" param)
           in [dcall]         
         _ -> [s]
