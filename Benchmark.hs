module Benchmark where

import Model 
import qualified Data.Map as M
import Data.Maybe

-- fib_bench_true
-- Counters: (923,923)
-- (769.94 secs, 129515603808 bytes)

-- fib_bench_false
t1_fib, t2_fib :: Transition
t1_fib = ("p", "t1", t1_fib')
t2_fib = ("q", "t2", t2_fib')

num = 11

t1_fib', t2_fib' :: Sigma -> Maybe Sigma
t1_fib' s = case M.lookup "k1" s of
  Nothing -> error "cant happen"
  Just k  -> 
    if k < num
    then 
      let i = fromMaybe (error "cant happen") $ M.lookup "i" s
          j = fromMaybe (error "cant happen") $ M.lookup "j" s
          j' = i + j
          check = i >= 46368 || j' >= 46368
      in if check
         then error "ASSERT FAIL"
         else Just $ M.insert "k1" (k+1) $ M.insert "j" j' s 
    else Nothing

t2_fib' s = case M.lookup "k2" s of
  Nothing -> error "cant happen"
  Just k  -> 
    if k < num
    then 
      let i = fromMaybe (error "cant happen") $ M.lookup "i" s
          j = fromMaybe (error "cant happen") $ M.lookup "j" s
          i' = i + j
          check = j >= 46368 || i' >= 46368
      in if check
         then error "ASSERT FAIL"
         else Just $ M.insert "k2" (k+1) $ M.insert "i" i' s 
    else Nothing

s_fib_bench_false :: Sigma
s_fib_bench_false = M.insert "i" 1 $ M.insert "j" 1 $ M.insert "k1" 0 $ M.singleton "k2" 0

fib_bench_false :: System
fib_bench_false = ([t1_fib, t2_fib], s_fib_bench_false)

ind_fib_bench_false :: UIndependence
ind_fib_bench_false = []

-- szymanski_true: non acyclic state space
t1_1_syz = ("p", "t1_1", t11')
t1_2_syz = ("p", "t1_2", t12')
t1_3_syz = ("p", "t1_3", t13')
t1_4_syz = ("p", "t1_4", t14')
t1_5_syz = ("p", "t1_5", t15')
t1_6_syz = ("p", "t1_6", t16')
t1_7_syz = ("p", "t1_7", t17')
t1_8_syz = ("p", "t1_8", t18')
t1_9_syz = ("p", "t1_9", t19')
t1_10_syz = ("p", "t1_10", t110')
t1_11_syz = ("p", "t1_11", t111')
t1_12_syz = ("p", "t1_12", t112')
t1_13_syz = ("p", "t1_13", t113')
t2_1_syz = ("q", "t2_1", t21')
t2_2_syz = ("q", "t2_2", t22')
t2_3_syz = ("q", "t2_3", t23')
t2_4_syz = ("q", "t2_4", t24')
t2_5_syz = ("q", "t2_5", t25')
t2_6_syz = ("q", "t2_6", t26')
t2_7_syz = ("q", "t2_7", t27')
t2_8_syz = ("q", "t2_8", t28')
t2_9_syz = ("q", "t2_9", t29')
t2_10_syz = ("q", "t2_10", t210')
t2_11_syz = ("q", "t2_11", t211')
t2_12_syz = ("q", "t2_12", t212')
t2_13_syz = ("q", "t2_13", t213')

t1s = map (\i -> "t1_" ++ show i) [1..13]
t2s = map (\i -> "t2_" ++ show i) [1..13]

ind_syzmanski_true = [(x,y) | x <- t1s, y <- t2s, not $ (x,y) `elem` dep_szymanski_true] 
dep_szymanski_true = 
  [ ("t1_2", "t2_3")
  , ("t1_2", "t2_5")
  , ("t1_2", "t2_7")
  , ("t1_2", "t2_9")
  , ("t1_2", "t2_12")
  , ("t1_4", "t2_3")
  , ("t1_4", "t2_5")
  , ("t1_4", "t2_7")
  , ("t1_4", "t2_9")
  , ("t1_4", "t2_12")
  , ("t1_6", "t2_3")
  , ("t1_6", "t2_5")
  , ("t1_6", "t2_7")
  , ("t1_6", "t2_9")
  , ("t1_6", "t2_12")
  , ("t1_8", "t2_3")
  , ("t1_8", "t2_5")
  , ("t1_8", "t2_7")
  , ("t1_8", "t2_9")
  , ("t1_8", "t2_12")
  , ("t1_13", "t2_3")
  , ("t1_13", "t2_5")
  , ("t1_13", "t2_7")
  , ("t1_13", "t2_9")
  , ("t1_13", "t2_12")
  , ("t2_2", "t1_3")
  , ("t2_2", "t1_5")
  , ("t2_2", "t1_7")
  , ("t2_2", "t1_9")
  , ("t2_2", "t1_12")
  , ("t2_4", "t1_3")
  , ("t2_4", "t1_5")
  , ("t2_4", "t1_7")
  , ("t2_4", "t1_9")
  , ("t2_4", "t1_12")
  , ("t2_6", "t1_3")
  , ("t2_6", "t1_5")
  , ("t2_6", "t1_7")
  , ("t2_6", "t1_9")
  , ("t2_6", "t1_12")
  , ("t2_8", "t1_3")
  , ("t2_8", "t1_5")
  , ("t2_8", "t1_7")
  , ("t2_8", "t1_9")
  , ("t2_8", "t1_12")
  , ("t2_13", "t1_3")
  , ("t2_13", "t1_5")
  , ("t2_13", "t1_7")
  , ("t2_13", "t1_9")
  , ("t2_13", "t1_12")
  , ("t1_10", "t2_10")
  , ("t1_10", "t2_11")
  , ("t2_11", "t1_10")
  ]

t11' s = 
  let pc = fromMaybe (error "pcp t1") $ M.lookup "pcp" s
  in if pc == 1
     then Just $ M.insert "pcp" 2 s
     else Nothing
t12' s =
  let pc = fromMaybe (error "pcp t2") $ M.lookup "pcp" s
  in if pc == 2
     then Just $ M.insert "pcp" 3 $ M.insert "flag1" 1 s
     else Nothing
t13' s =
  let pc = fromMaybe (error "pcp t3") $ M.lookup "pcp" s
      flag2 = fromMaybe (error "flag2 t3") $ M.lookup "flag2" s
  in if pc == 3
     then if flag2 >= 3
          then Just s 
          else Just $ M.insert "pcp" 4 s
     else Nothing
t14' s =
  let pc = fromMaybe (error "pcp t4") $ M.lookup "pcp" s
  in if pc == 4
     then Just $ M.insert "pcp" 5 $ M.insert "flag1" 3 s
     else Nothing
t15' s =
  let pc = fromMaybe (error "pcp t5") $ M.lookup "pcp" s
      flag2 = fromMaybe (error "flag2 t5") $ M.lookup "flag2" s
  in if pc == 5
     then if flag2 == 1
          then Just $ M.insert "pcp" 6 s
          else Just $ M.insert "pcp" 8 s
     else Nothing
t16' s =
  let pc = fromMaybe (error "pcp t6") $ M.lookup "pcp" s
  in if pc == 6
     then Just $ M.insert "pcp" 7 $ M.insert "flag1" 2 s
     else Nothing
t17' s =
  let pc = fromMaybe (error "pcp t7") $ M.lookup "pcp" s
      flag2 = fromMaybe (error "flag2 t7") $ M.lookup "flag2" s
  in if pc == 7
     then if flag2 /= 4
          then Just s 
          else Just $ M.insert "pcp" 8 s
     else Nothing
t18' s =
  let pc = fromMaybe (error "pcp t8") $ M.lookup "pcp" s
  in if pc == 8
     then Just $ M.insert "pcp" 9 $ M.insert "flag1" 4 s
     else Nothing
t19' s =
  let pc = fromMaybe (error "pcp t9") $ M.lookup "pcp" s
      flag2 = fromMaybe (error "flag2 t9") $ M.lookup "flag2" s
  in if pc == 9
     then if flag2 >= 2
          then Just s 
          else Just $ M.insert "pcp" 10 s
     else Nothing
t110' s =
  let pc = fromMaybe (error "pcp t10") $ M.lookup "pcp" s
  in if pc == 10
     then Just $ M.insert "x" 0 $ M.insert "pcp" 11 s
     else Nothing
t111' s =
  let pc = fromMaybe (error "pcp t11") $ M.lookup "pcp" s
      x = fromMaybe (error "x t11") $ M.lookup "x" s
  in if pc == 11
     then if x <= 0
          then Just $ M.insert "pcp" 12 s
          else error "ASSERT X <= 0 FAIL t11"
     else Nothing
t112' s =
  let pc = fromMaybe (error "pcp t12") $ M.lookup "pcp" s
      flag2 = fromMaybe (error "flag2 t12") $ M.lookup "flag2" s
  in if pc == 12
     then if 2 <= flag2 && flag2 <= 3
          then Just s 
          else Just $ M.insert "pcp" 13 s
     else Nothing
t113' s =
  let pc = fromMaybe (error "pcp t13") $ M.lookup "pcp" s
  in if pc == 13
     then Just $ M.insert "flag1" 0 $ M.insert "pcp" 1 s
     else Nothing

t21' s = 
  let pc = fromMaybe (error "pcq t1") $ M.lookup "pcq" s
  in if pc == 1
     then Just $ M.insert "pcq" 2 s
     else Nothing
t22' s =
  let pc = fromMaybe (error "pcq t2") $ M.lookup "pcq" s
  in if pc == 2
     then Just $ M.insert "pcq" 3 $ M.insert "flag2" 1 s
     else Nothing
t23' s =
  let pc = fromMaybe (error "pcq t3") $ M.lookup "pcq" s
      flag1 = fromMaybe (error "flag1 t3") $ M.lookup "flag1" s
  in if pc == 3
     then if flag1 >= 3
          then Just s 
          else Just $ M.insert "pcq" 4 s
     else Nothing
t24' s =
  let pc = fromMaybe (error "pcq t4") $ M.lookup "pcq" s
  in if pc == 4
     then Just $ M.insert "pcq" 5 $ M.insert "flag2" 3 s
     else Nothing
t25' s =
  let pc = fromMaybe (error "pcq t5") $ M.lookup "pcq" s
      flag1 = fromMaybe (error "flag1 t5") $ M.lookup "flag1" s
  in if pc == 5
     then if flag1 == 1
          then Just $ M.insert "pcq" 6 s
          else Just $ M.insert "pcq" 8 s
     else Nothing
t26' s =
  let pc = fromMaybe (error "pcq t6") $ M.lookup "pcq" s
  in if pc == 6
     then Just $ M.insert "pcq" 7 $ M.insert "flag2" 2 s
     else Nothing
t27' s =
  let pc = fromMaybe (error "pcq t7") $ M.lookup "pcq" s
      flag1 = fromMaybe (error "flag1 t7") $ M.lookup "flag1" s
  in if pc == 7
     then if flag1 /= 4
          then Just s 
          else Just $ M.insert "pcq" 8 s
     else Nothing
t28' s =
  let pc = fromMaybe (error "pcq t8") $ M.lookup "pcq" s
  in if pc == 8
     then Just $ M.insert "pcq" 9 $ M.insert "flag2" 4 s
     else Nothing
t29' s =
  let pc = fromMaybe (error "pcq t9") $ M.lookup "pcq" s
      flag1 = fromMaybe (error "flag1 t9") $ M.lookup "flag1" s
  in if pc == 9
     then if flag1 >= 2
          then Just s 
          else Just $ M.insert "pcq" 10 s
     else Nothing
t210' s =
  let pc = fromMaybe (error "pcq t10") $ M.lookup "pcq" s
  in if pc == 10
     then Just $ M.insert "x" 1 $ M.insert "pcq" 11 s
     else Nothing
t211' s =
  let pc = fromMaybe (error "pcq t11") $ M.lookup "pcq" s
      x = fromMaybe (error "x t11") $ M.lookup "x" s
  in if pc == 11
     then if x >= 1
          then Just $ M.insert "pcq" 12 s
          else error "ASSERT X >= 1 FAIL t11"
     else Nothing
t212' s =
  let pc = fromMaybe (error "pcq t12") $ M.lookup "pcq" s
      flag1 = fromMaybe (error "flag1 t12") $ M.lookup "flag1" s
  in if pc == 12
     then if 2 <= flag1 && flag1 <= 3
          then Just s 
          else Just $ M.insert "pcq" 13 s
     else Nothing
t213' s =
  let pc = fromMaybe (error "pcq t13") $ M.lookup "pcq" s
  in if pc == 13
     then Just $ M.insert "flag2" 0 $ M.insert "pcq" 1 s
     else Nothing

s_syzmanski_true :: Sigma
s_syzmanski_true = M.insert "flag1" 0 $ M.insert "flag2" 0 $ M.insert "pcp" 1 $ M.singleton "pcq" 1

sys_syzmanksi_true :: System
sys_syzmanksi_true = ([t1_1_syz,t1_2_syz,t1_3_syz,t1_4_syz,t1_5_syz,t1_6_syz,t1_7_syz,t1_8_syz,t1_9_syz,t1_10_syz,t1_11_syz,t1_12_syz,t1_13_syz,t2_1_syz,t2_2_syz,t2_3_syz,t2_4_syz,t2_5_syz,t2_6_syz,t2_7_syz,t2_8_syz,t2_9_syz,t2_10_syz,t2_11_syz,t2_12_syz,t2_13_syz], s_syzmanski_true)
