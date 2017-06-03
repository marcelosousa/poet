-------------------------------------------------------------------------------
-- Module    :  Model.Interpreter
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- Interpreter with multiple modes for POET
--  Multiple modes for single execution
--  with different scheduling strategies; 
--  Basic exploration ideas
-------------------------------------------------------------------------------
module Model.Interpreter (interpreter, toIPMode) where

import Data.Hashable
import Data.HashTable.IO hiding (mapM_)
import Data.Map (Map)
import Domain.Action
import Domain.Class
import Domain.Concrete
import Domain.Interval
import Domain.Lattice
import Model.GCS
import System.IO.Unsafe
import System.Random
import Util.Generic
import qualified Data.HashTable.IO as H hiding (mapM_)
import qualified Data.Map as M

-- @TODO: Need to define STATE OF THE INTERPRETER

toIPMode :: Int -> Analysis -> Int -> IPMode
toIPMode 0 a _ = IPSingle     a
toIPMode 1 a n = IPSingleRand a n
toIPMode 2 a _ = IPDFS        a
toIPMode 3 a _ = IPDFST       a
toIPMode 4 a _ = IPBFS        a

data IPMode = IPSingle     Analysis 
            | IPSingleRand Analysis Int 
            | IPDFS        Analysis
            | IPDFST       Analysis
            | IPBFS        Analysis

  deriving (Eq,Show)

-- | Main interpreter function:
--    Depending on the mode, select various back-ends
interpreter :: (Hashable s, Domain s a) => System s a -> IPMode -> Bool -> Bool -> Int -> IO ()
interpreter syst mode stl cut wid = 
  case mode of 
    IPSingle _ -> execute  mode syst (gbst syst)
    IPBFS    _ -> bfs      mode syst (gbst syst)
    IPDFS    _ -> do 
      stats <- H.new
      dfs 0 1 1 [] stats syst [(0,gbst syst)]
--      dfs 1 1 [] M.empty syst [(0, gbst syst)]
    IPDFST   _ -> dfs_tree 1 [] mode syst [(0, gbst syst)]

execute :: Domain s a => IPMode -> System s a -> s -> IO ()
execute mode syst st = do
  case enabled syst st of
    []    -> do 
      putStrLn "executor: end of trace"
      putStrLn $ "executor: " ++ foldr (\(tid,pos) r -> " \ttid = " ++ show tid ++ " \tpos = " ++ show pos ++ " \t " ++ r) "" (M.toList $ controlPart st)
    ts -> do
      let ths = fst $ unzip ts
      putStrLn $ pp (symt syst) st
      putStrLn $ "Enabled threads: " ++ show ths 
      putStrLn $ "Pick a thread: "
      num <- getLine 
      let (t,s) = head ts -- read num :: Int
      if t `elem` ths
      then execute mode syst s
      else putStrLn $ "executor: invalid tid, aborting!"
        
get_pre :: Projection s => s -> Int -> Int
get_pre s pid = 
  case M.lookup pid (controlPart s) of
    Nothing -> error $ "get_pre: no information for " ++ show pid
    Just p  -> p

bfs :: Domain s a => IPMode -> System s a -> s -> IO ()
bfs mode syst st = do
  case enabled syst st of
    []    -> do 
      putStrLn "bfs: end of trace"
      putStrLn $ "bfs: " ++ foldr (\(tid,pos) r -> " \ttid = " ++ show tid ++ " \tpos = " ++ show pos ++ "\t" ++ r) "" (M.toList $ controlPart st)
    ts -> error "TODO!" -- bfs mode syst s

-- | Depth First Search over the computation tree;
dfs_tree :: Domain s a => Int -> [Int] -> IPMode -> System s a -> [(Int,s)] -> IO ()
dfs_tree n stack mode syst [] = do
  putStrLn $ "dfs_tree: end of exploration"
  putStrLn $ "dfs_tree: #traces  = " ++ show (n-1)
dfs_tree n stack mode syst ((i,s):ss) = do
  --putStrLn $ "dfs: tid = " ++ show i ++ " "
  --           ++ foldr (\(tid,pos) r -> " \t tid = " ++ show tid 
  --               ++ " \t pos = " ++ show pos ++ " \t " ++ r) "" (M.toList $ controlPart s)
  case enabled syst s of
    [] -> do       
      -- putStrLn $ "dfs: end of trace " ++ show n
      dfs_tree (n+1) stack mode syst ss
    ts -> do
      -- if length ts > 1 then putStrLn $ "dfs: branching " ++ show ts else return ()
      dfs_tree n (i:stack) mode syst (ts++ss)

--type States s   = Map s ()
type States s = CuckooHashTable Int s

-- | Standard Depth First Search
dfs :: (Hashable s, Domain s a) => Int -> Int -> Int -> [Int] -> States s -> System s a -> [(Int,s)] -> IO ()
dfs tid n m stack visited syst []         = do 
  putStrLn $ "dfs: end of exploration"
  putStrLn $ "dfs: #traces  = " ++ show (n-1)
  putStrLn $ "dfs: #cutoffs = " ++ show (m-1)
dfs tid n m stack visited syst ((i,s):ss) = do
  --putStrLn $ "dfs: tid = " ++ show i ++ " "
  --           ++ foldr (\(tid,pos) r -> " \t tid = " ++ show tid 
  --               ++ " \t pos = " ++ show pos ++ " \t " ++ r) "" (M.toList $ controlPart s)
  -- check <- if tid == i then return False else s `isCutoff` visited
  check <- s `isCutoff` visited
  if check
  then do
   -- putStrLn $ "dfs: cutoff " ++ show m
   -- putStrLn $ pp (symt syst) s
    dfs i n (m+1) stack visited syst ss
  else do
    case enabled syst s of
      [] -> do       
        -- putStrLn $ "dfs: end of trace " ++ show n
        dfs i (n+1) m stack visited syst ss
      ts -> do
        -- Version with Map
        -- dfs n m (i:stack) (M.insert s () visited) syst (ns++ss)                 
        -- if tid == i then return () else H.insert visited (hash s) s
        H.insert visited (hash s) s
        dfs i n m (i:stack) visited syst (ts++ss)

isCutoff :: (Hashable s, Ord s) => s -> States s -> IO Bool
isCutoff s states = do
  res <- H.lookup states (hash s)
  case res of
    Just s' -> return (s == s')
    Nothing -> return False
      
replay :: Domain s a => [Int] -> IPMode -> System s a -> s -> IO ()
replay []     mode syst st = execute mode syst st
replay (t:ts) mode syst st = do
  case run False 10000 syst st t of
    (warns,[(s,pos,a)]) -> do
      putStrLn $ "replay: tid = " ++ show t ++ " \tpos = " ++ show pos ++ " \t" ++ show a
      replay ts mode syst s
    (warns,r) -> error $ "replay: FATAL! tid = " ++ show t ++ " \t" ++ show (map (\(a,b,c) -> (b,c)) r)
  

{-
ai :: FilePath -> IO ()
ai f = do
  fe <- extract "" f
  let syst = IC.convert fe
      (warns,res) = run True 10 syst (gbst syst) 1
      -- get the symbol table
      sym_table = Language.SimpleC.symt fe 
--  putStrLn $ show (gbst syst) 
      fname = fst $ splitExtension f
      dot_name = fname ++ ".dot"
      pdf_name = fname ++ ".pdf"
  writeFile dot_name $ P.pp_dot_graphs (Language.SimpleC.cfgs fe) M.empty -- sym_table
  putStrLn $ show_symt sym_table 
  putStrLn $ "Warnings: " ++ show warns
  putStrLn $ showResultList res
  -- pdf <- readProcess "dot" ["-Tpdf",dot_name] []
  -- writeFile pdf_name pdf
  putStrLn "explore end"

execute :: FilePath -> Analysis -> Int -> IO ()
execute = error "v2: working in progress"
{-    
execute f dom dseed = do
  prog <- extract f
  seed <- randomIO :: IO Int
  print $ "Using seed = " ++ show (seed,dseed)
  let (prog', fflow, flow, thcount) = frontEnd prog
  print prog'
  let gen = if dseed == 0
            then mkStdGen seed
            else mkStdGen dseed
      log = case dom of 
        Concrete -> exec gen thcount $ CC.convert prog' fflow flow thcount
        Interval -> exec gen thcount $ IC.convert prog' fflow flow thcount
  putStrLn log
-}

exec :: (Show st, Ord st) => StdGen -> Int -> (System st, UIndep) -> String
exec gen thcount (sys,indep) = execIt gen thcount "" indep sys (initialState sys)

execIt :: (Show st, Ord st) => StdGen -> Int -> String -> UIndep -> System st -> st -> String
execIt gen thcount str indep sys st =
  let trs = enabledTransitions sys st
      ststr = show st
  in if V.null trs
     then 
       if isDeadlock thcount st
       then str ++ ststr
       else error $ "exec fatal: " ++ show (thcount+1) ++ "\n" ++ show ststr
     else 
      let ltrs = "Enabled transitions: " ++ (show $ V.toList trs)
          indepTr = getIndepTr indep $ V.toList trs
      in if checkDiamonds sys indepTr st
         then
           let (n, gen') = randomR (0,V.length trs - 1) gen
           in case trs V.!? n of
             Nothing -> error $ "execIt getTransition fail!"
             Just (procID,trID,_,_) ->
               let tr = getTransitionWithID sys trID
                   nstr = str ++ ststr ++ ltrs ++ "\nIndependent transitions=" ++ show indepTr ++ "\nRunning " ++ show (trID,procID) ++ "\n\n"
                   nst = head $ tr st
               in execIt gen' thcount nstr indep sys nst
      else error "diamond check failed"
          
checkDiamonds :: Ord st => System st -> [(TransitionInfo,TransitionInfo)] -> st -> Bool
checkDiamonds sys [] s = True
checkDiamonds sys (((_,t1,_,_),(_,t2,_,_)):rest) s =
  if checkDiamond sys t1 t2 s
  then checkDiamonds sys rest s
  else error $ "checkDiamonds: " ++ show (t1,t2)

checkDiamond :: Ord st => System st -> TransitionID -> TransitionID -> st -> Bool
checkDiamond sys t1 t2 s =
    let t1fn = getTransition sys t1
        t2fn = getTransition sys t2
        s1s2 = sort $ map t1fn $ t2fn s
        s2s1 = sort $ map t2fn $ t1fn s
    in s1s2 == s2s1     
            
isDeadlock :: Int -> st -> Bool
isDeadlock thcount st =  True 
{-
FIXME: Check that all pcs are in -1
do
    let ident = BS.pack "__poet_mutex_death"
    _ <- safeLookup "isDeadlock" st ident
    case mlock of
        Just (Var locks) -> return $ length locks == (thcount+1)
        _ -> error $ "isDeadlock: " ++ show mlock
 -}       

interpret :: (Show st, Ord st) => (System st, UIndep) -> Int
interpret (sys,indep) = interpretIt 0 indep sys (initialState sys)

interpretIt :: Show st => Int -> UIndep -> System st -> st -> Int
interpretIt step indep sys st =
  let trs = enabledTransitions sys st
      ststr = show st
      indepTr = getIndepTr indep $ V.toList trs
      s1 = unsafePerformIO $ putStrLn "\n========================================\nCurrent state:"
      s2 = unsafePerformIO $ putStrLn ststr
      s3 = unsafePerformIO $ putStrLn menu
      s4 = unsafePerformIO $ print trs
      s5 = unsafePerformIO $ putStrLn $ "\nIndependent transitions: " ++ show indepTr
  in if s1 `seq` s2 `seq` s3 `seq` s4 `seq` s5 `seq` V.null trs
     then
       let f = unsafePerformIO $ print "Finished execution"
       in f `seq` step
     else
       let c = unsafePerformIO $ getChar
           n = case c of
             'a' -> 0
             'x' -> (-1)
             _ -> digitToInt c
       in if n >= 0
          then
            case trs V.!? n of
              Nothing -> error $ "interpret getTransition fail: " ++ show n
              Just (_,trID,_,_) ->
                let tr = getTransitionWithID sys trID
                    nst = head $ tr st
                in interpretIt (step+1) indep sys nst
          else step

menu :: String
menu = "\nChoose an enabled transition (by the position in the list, 'x' to quit):"
--menu = unlines ["Choose a transition by position:", "(a): automatic mode", "(n): number of enabled transition", "(x): exit"]
-}
