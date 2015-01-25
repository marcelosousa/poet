module Model.Converter where

import Language.SimpleC.AST
--import SimpleC.Converter
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Vector as V

import Debug.Trace

import Language.C.Syntax.AST (CBinaryOp(..),CUnaryOp(..))
import qualified Model.Language as L

--myMain = do
--    ctu <- parseMyFile "/Users/mabs/Research/tools/unfolds/examples/simple.c"
--    let ast = translate ctu
--        model = converter ast
--    print $ L.exec model

{-
- Assumptions:
-  1) Everything that is not main is a thread.
-  2) The main function simply creates threads and joins them.
-  3) All function calls are inlined. Hence, the function 
-     definitions that we have are threads.
-  4) Moreover, the names of all local variables are disjoint.
-}    

convert :: Program -> L.System
convert = undefined
{-
converter (Program (decls, defs)) = 
    let threads = getThreads defs
        i = foldr initState L.emptyState decls
        (tr, fi) = foldr convertThread (M.empty, i) threads
    in L.System fi tr

-- Fine
getThreads :: Defs -> Defs
getThreads = filter notMain 
  where notMain (FunctionDef _ name _ _) = name /= "main"

-- Build the initial state of the system
-- based on the nr of threads and locals 
-- declared in the threads.
initState :: Declaration -> L.State -> L.State
initState decl s@(val,ps) = case decl of
    FunctionDecl _ _ _         -> s
    GlobalDecl _ i Nothing     -> (M.insert i 0 val,ps) -- Default value is 0
    GlobalDecl _ i (Just mval) -> (M.insert i (convertValue mval) val, ps)

convertValue :: Value -> L.Value
convertValue (IntValue i) = fromInteger i
convertValue _ = error "cant convert value"

convertThread :: Definition -> (L.TransitionSystem, L.State) -> (L.TransitionSystem, L.State)
convertThread (FunctionDef _ name _ stat) (ptr, (val,procs)) =
    let procs' = name:procs
        pcName = "pc"++name -- pcProc
        tr = M.toList stat
        val' = case getFirst tr of
            Nothing -> error "no statement in func"
            Just pc -> M.insert pcName pc val
        nptr = convertStat pcName Nothing tr
        ptr' = M.insert name nptr ptr
    in (ptr', (val',procs'))
                     
getFirst :: [(PC,Statement)] -> Maybe PC
getFirst []         = Nothing
getFirst ((pc,_):_) = Just pc

convertStat :: L.Var -> [(PC,Statement)] -> L.Transition
convertStat pcName stats = 
    \s@(val,procs) -> 
      let pc = L.slookup pcName val
      in case filter (hasPC pc) stats of
          [] -> trace "convertStat no PC" $ (s, V.empty)
          [(_,st)] -> convertStatement pcName st s


hasPC :: PC -> (PC,Statement) -> Bool
hasPC pc (pc',_) = pc == pc'

convertStat pcn _         []     = \s -> (s, V.empty)
convertStat pcn Nothing   [stat] = convertStatement pcn stat (-1) 
convertStat pcn (Just pc) [stat] = convertStatement pcn stat pc
convertStat pcn mpc (s1:(pc,s2):r) =
    let tr1 = convertStatement pcn s1 pc
        tr2 = convertStat pcn mpc ((pc,s2):r)
    in \s -> let (s1,tags1) = tr1 s
                 (sr,tagsr) = tr2 s1
             in (sr, tags1 V.++ tagsr)

-- type Transition = State -> (State, V.Vector Tag)
-- type State = (M.Map Var Value, [Var])
convertStatement :: L.Var -> PC -> Statement -> L.Transition
convertStatement pcName pcNext stat = case stat of
    Assign i expr -> \s@(val, procs) ->
      let (v, tags') = eval expr val
          tags = V.cons (L.Write i) tags'
          val' = M.adjust (const v) i $ M.adjust (const pcNext) pcName val
      in ((val', procs), tags)

convertStatement :: L.Var -> (PC,Statement) -> PC -> L.Transition
convertStatement pcn (pc,stat) pca = case stat of
    Assign i expr -> \s@(val, procs) ->
      let cpcn = L.slookup pcn val
      in if cpcn == pc
         then let (v, tags') = eval expr val
                  tags = V.cons (L.Write i) tags'
                  val' = M.adjust (const v) i $ M.adjust (const pca) pcn val
              in ((val',procs), tags)
         else (s,V.empty)-- error $ "undefined in assign " ++ show stat ++ " " ++ show (cpcn, pc)
    If cond sThen sElse -> 
      let iThen = M.toList sThen
          iElse = M.toList sElse
          (pct,tTrans) = (getFirst iThen, convertStat pcn (Just pca) iThen)
          (pce,eTrans) = (getFirst iElse, convertStat pcn (Just pca) iElse)
      in \(val, procs) -> 
          if L.slookup pcn val == pc
          then let (v, tags) = eval cond val
               in if v==1
                  then let npc = fromMaybe pca pct 
                           val' = M.adjust (const npc) pcn val
                           (s, tags') = tTrans (val', procs)
                       in (s, tags V.++ tags')                      
                  else let npc = fromMaybe pca pce 
                           val' = M.adjust (const npc) pcn val
                           (s, tags') = eTrans (val', procs)
                       in (s, tags V.++ tags')
          else (s,V.empty) --error "undefined in if"
    While cond stats -> 
      let iBody = M.toList stats
          (pcbody,body) = (getFirst iBody, convertStat pcn (Just pca) iBody)
      in \(val, procs) -> 
           if L.slookup pcn val == pc
           then let (v, tags) = eval cond val
              in if v == 1
                 then let npc = fromMaybe pca pcbody
                          val' = M.adjust (const npc) pcn val
                          (s, tags') = body (val', procs)
                      in (s, tags V.++ tags')
                 else ((M.adjust (const pca) pcn val, procs), tags)
           else (s,V.empty)--error "undefined in while"
    Return expr -> \(val, procs) -> 
      if L.slookup pcn val == pc
      then ((val, filter (/=pcn) procs), V.empty) -- the return doesnt have effect
      else (s,V.empty)--error "undefined in return"
    CallS fn args -> error "no support for call stat"
    Local i -> \(val, procs) -> 
      if L.slookup pcn val == pc
      then let val' = M.adjust (const pca) pcn $ M.insert i 0 val
               tags = V.singleton $ L.Write i
           in ((val', procs), tags)
      else error "undefined in local"

eval :: Expression -> L.Valuation -> (L.Value, V.Vector L.Tag)
eval expr env = case expr of
    Call i args -> error "no support for call expr"
    BinOp opCode e1 e2 -> 
      let (v1,t1) = eval e1 env
          (v2,t2) = eval e2 env
      in case opCode of
          CMulOp -> (v1 * v2, t1 V.++ t2)
          CDivOp -> (div v1 v2, t1 V.++ t2)
          CRmdOp -> (mod v1 v2, t1 V.++ t2)
          CAddOp -> (v1 + v2, t1 V.++ t2)    
          CSubOp -> (v1 - v2, t1 V.++ t2)    
          CLeOp	 -> (boolToInt $ v1 < v2, t1 V.++ t2)    
          CGrOp	 -> (boolToInt $ v1 > v2, t1 V.++ t2)    
          CLeqOp -> (boolToInt $ v1 <= v2, t1 V.++ t2)       
          CGeqOp -> (boolToInt $ v1 >= v2, t1 V.++ t2)       
          CEqOp	 -> (boolToInt $ v1 == v2, t1 V.++ t2)    
          CNeqOp -> (boolToInt $ v1 /= v2, t1 V.++ t2)       
          _ -> error "not supported binop"    
    UnaryOp opCode e -> 
      let (v,t) = eval e env
      in case opCode of 
          CPreIncOp	-> (v + 1,t)
          CPreDecOp	-> (v - 1,t)
          CPostIncOp -> (v + 1,t)	
          CPostDecOp -> (v - 1,t)
          CNegOp -> if v == 0
                    then (1,t)
                    else (0,t)
          _ -> error "not supported unop"
    Const v -> (convertValue v, V.empty)
    Ident i -> (L.slookup i env, V.singleton $ L.Read i)

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0
-}