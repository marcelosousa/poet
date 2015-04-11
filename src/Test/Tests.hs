module Test.Tests where

import Exploration.UNF.APIStateless
import Exploration.UNF.Unfolderless
import Test.Examples
import Test.Examples.ExOne
import Test.Examples.ExTwo
import Test.Examples.ExThree
import Test.Examples.ExFour
import Test.Examples.ExFive
import Test.Examples.ExSix
import Test.Examples.ExSeven
import Test.Examples.ExEight

import Control.Monad.ST.Safe
import Test.HUnit

test1 = 
  let r1 = runST (sys1 >>= \sys -> stateless False sys ind11 >>= return . show)
  in TestCase (assertEqual "WxWx" "(5,2)" r1)

test2 = 
  let r1 = runST (sys1 >>= \sys -> stateless False sys ind12 >>= return . show)
  in TestCase (assertEqual "WxWy" "(3,1)" r1)

test3 = 
  let r1 = runST (sys2 >>= \sys -> stateless False sys ind2 >>= return . show)
  in TestCase (assertEqual "WxRxRx" "(11,4)" r1)

test4 = 
  let r1 = runST (sys3 >>= \sys -> stateless False sys ind3 >>= return . show)
  in TestCase (assertEqual "paper" "(13,4)" r1)

test5 = 
  let r1 = runST (sys4 >>= \sys -> stateless False sys ind4 >>= return . show)
  in TestCase (assertEqual "ex4" "(7,2)" r1)

test6 = 
  let r1 = runST (sys5 >>= \sys -> stateless False sys ind5 >>= return . show)
  in TestCase (assertEqual "cesar" "(12,4)" r1)

test7 = 
  let r1 = runST (sys6 >>= \sys -> stateless False sys ind6 >>= return . show)
  in TestCase (assertEqual "histories" "(15,4)" r1)

test8  = 
  let r1 = runST (sys1 >>= \sys -> stateless True sys ind11 >>= return . show . maxConf)
      r2 = runST (sys1 >>= \sys -> stateless False sys ind11 >>= return . show . maxConf)
  in TestCase (assertEqual "WxWx" r1 r2)

test9 = 
  let r1 = runST (sys1 >>= \sys -> stateless True sys ind12 >>= return . show . maxConf)
      r2 = runST (sys1 >>= \sys -> stateless False sys ind12 >>= return . show . maxConf)
  in TestCase (assertEqual "WxWy" r1 r2)

test10 = 
  let r1 = runST (sys2 >>= \sys -> stateless True sys ind2 >>= return . show . maxConf)
      r2 = runST (sys2 >>= \sys -> stateless False sys ind2 >>= return . show . maxConf)
  in TestCase (assertEqual "WxRxRx" r1 r2)

test11 = 
  let r1 = runST (sys3 >>= \sys -> stateless True sys ind3 >>= return . show . maxConf)
      r2 = runST (sys3 >>= \sys -> stateless False sys ind3 >>= return . show . maxConf)
  in TestCase (assertEqual "paper" r1 r2)

test12 = 
  let r1 = runST (sys4 >>= \sys -> stateless True sys ind4 >>= return . show . maxConf)
      r2 = runST (sys4 >>= \sys -> stateless False sys ind4 >>= return . show . maxConf)
  in TestCase (assertEqual "ex4" r1 r2)

test13 = 
  let r1 = runST (sys5 >>= \sys -> stateless True sys ind5 >>= return . show . maxConf)
      r2 = runST (sys5 >>= \sys -> stateless False sys ind5 >>= return . show . maxConf)
  in TestCase (assertEqual "cesar" r1 r2)

test14 = 
  let r1 = runST (sys6 >>= \sys -> stateless True sys ind6 >>= return . show . maxConf)
      r2 = runST (sys6 >>= \sys -> stateless False sys ind6 >>= return . show . maxConf)
  in TestCase (assertEqual "histories" r1 r2)

-- locks tests
test15 = 
  let r1 = runST (sys7 >>= \sys -> stateless False sys ind7 >>= return . show)
  in TestCase (assertEqual "lock-simple" "(13,2)" r1)

test16 = 
  let r1 = runST (sys7 >>= \sys -> stateless True sys ind7 >>= return . show . maxConf)
      r2 = runST (sys7 >>= \sys -> stateless False sys ind7 >>= return . show . maxConf)
  in TestCase (assertEqual "lock-simple-fullvsless" r1 r2)

test17 = 
  let r1 = runST (sys8 >>= \sys -> stateless False sys ind8 >>= return . show)
  in TestCase (assertEqual "unlock-simple" "(9,2)" r1)

test18 = 
  let r1 = runST (sys8 >>= \sys -> stateless True sys ind8 >>= return . show . maxConf)
      r2 = runST (sys8 >>= \sys -> stateless False sys ind8 >>= return . show . maxConf)
  in TestCase (assertEqual "unlock-simple-fullvsless" r1 r2)


tests = TestList 
  [ TestLabel "test1" test1
  , TestLabel "test2" test2
  , TestLabel "test3" test3
  , TestLabel "test4" test4
  , TestLabel "test5" test5
  , TestLabel "test6" test6
  , TestLabel "test7" test7
  , TestLabel "test8" test8
  , TestLabel "test9" test9 
  , TestLabel "test10" test10
  , TestLabel "test11" test11 
  , TestLabel "test12" test12 
  , TestLabel "test13" test13 
  , TestLabel "test14" test14 
  , TestLabel "test15" test15
  , TestLabel "test16" test16
  , TestLabel "test17" test17
  , TestLabel "test18" test18
  ]
