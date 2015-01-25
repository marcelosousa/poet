module Tests where

import Unfolderless
import Examples

import Control.Monad.ST.Safe
import Test.HUnit

test1 = 
  let r1 = runST (sys1 >>= \sys -> stateless sys ind11 >>= return . show)
  in TestCase (assertEqual "WxWx" "5" r1)

test2 = 
  let r1 = runST (sys1 >>= \sys -> stateless sys ind12 >>= return . show)
  in TestCase (assertEqual "WxWy" "3" r1)

test3 = 
  let r1 = runST (sys2 >>= \sys -> stateless sys ind2 >>= return . show)
  in TestCase (assertEqual "WxRxRx" "11" r1)

test4 = 
  let r1 = runST (sys3 >>= \sys -> stateless sys ind3 >>= return . show)
  in TestCase (assertEqual "paper" "13" r1)

test5 = 
  let r1 = runST (sys4 >>= \sys -> stateless sys ind4 >>= return . show)
  in TestCase (assertEqual "ex4" "7" r1)

test6 = 
  let r1 = runST (sys5 >>= \sys -> stateless sys ind5 >>= return . show)
  in TestCase (assertEqual "cesar" "12" r1)

test7 = 
  let r1 = runST (sys6 >>= \sys -> stateless sys ind6 >>= return . show)
  in TestCase (assertEqual "histories" "15" r1)

tests = TestList 
  [ TestLabel "test1" test1
  , TestLabel "test2" test2 
  , TestLabel "test3" test3 
  , TestLabel "test4" test4 
--  , TestLabel "test5" test5 
--  , TestLabel "test6" test6 
--  , TestLabel "test7" test7 
  ] 
