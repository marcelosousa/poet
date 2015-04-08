module Test.Tests where

import Exploration.UNF.Unfolderless
import Test.Examples
import Test.Examples.ExOne
import Test.Examples.ExTwo
import Test.Examples.ExThree
import Test.Examples.ExFour
import Test.Examples.ExFive
import Test.Examples.ExSix

import Control.Monad.ST.Safe
import Test.HUnit

test1 mode = 
  let r1 = runST (sys1 >>= \sys -> stateless mode sys ind11 >>= return . show)
  in TestCase (assertEqual "WxWx" "5" r1)

test2 mode = 
  let r1 = runST (sys1 >>= \sys -> stateless mode sys ind12 >>= return . show)
  in TestCase (assertEqual "WxWy" "3" r1)

test3 mode = 
  let r1 = runST (sys2 >>= \sys -> stateless mode sys ind2 >>= return . show)
  in TestCase (assertEqual "WxRxRx" "11" r1)

test4 mode = 
  let r1 = runST (sys3 >>= \sys -> stateless mode sys ind3 >>= return . show)
  in TestCase (assertEqual "paper" "13" r1)

test5 mode = 
  let r1 = runST (sys4 >>= \sys -> stateless mode sys ind4 >>= return . show)
  in TestCase (assertEqual "ex4" "7" r1)

test6 mode = 
  let r1 = runST (sys5 >>= \sys -> stateless mode sys ind5 >>= return . show)
  in TestCase (assertEqual "cesar" "12" r1)

test7 mode = 
  let r1 = runST (sys6 >>= \sys -> stateless mode sys ind6 >>= return . show)
  in TestCase (assertEqual "histories" "15" r1)


tests2 mode = TestList 
  [ TestLabel "test1" (test1 mode)
  , TestLabel "test2" (test2 mode)
  , TestLabel "test3" (test3 mode)
  , TestLabel "test4" (test4 mode)
  , TestLabel "test5" (test5 mode)
  , TestLabel "test6" (test6 mode)
  , TestLabel "test7" (test7 mode)
  ]
  
tests mode = TestList 
  [ TestLabel "test3" (test3 mode)
  ] 