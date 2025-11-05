module Spec_Day03 (day03) where
    
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Day03

day03 :: [Test.Framework.Test]
day03 = 
  [ testPart1 "1" [E] 2
  , testPart1 "2" [N,E,S,W] 4
  , testPart1 "3" [N,S,N,S,N,S,N,S,N,S] 2
  , testPart2 "1" [N,S] 3
  , testPart2 "2" [N,E,S,W] 3
  , testPart2 "3" [N,S,N,S,N,S,N,S,N,S] 11
  ]

testPart1 :: String -> [Dirn] -> Int -> Test.Framework.Test
testPart1 = testN "Day 03, Part 1: " part1

testPart2 :: String -> [Dirn] -> Int -> Test.Framework.Test
testPart2 = testN "Day 03, Part 2: " part2

testN :: String -> (a -> Int) -> String -> a -> Int -> Test.Framework.Test
testN c f s xs x = testCase (c <> s) (f xs @?= x)