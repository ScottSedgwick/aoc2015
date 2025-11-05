module Spec_Day02 (day02) where
    
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Day02

day02 :: [Test.Framework.Test]
day02 = 
  [ testPart1 "1" (2,3,4) 58
  , testPart1 "2" (1,1,10) 43
  , testPart2 "1" (2,3,4) 34
  , testPart2 "2" (1,1,10) 14
  ]

testPart1 :: String -> (Int, Int, Int) -> Int -> Test.Framework.Test
testPart1 s xs x = testCase ("Day 02, Part 1: " <> s) (part1' xs @?= x)

testPart2 :: String -> (Int, Int, Int) -> Int -> Test.Framework.Test
testPart2 s xs x = testCase ("Day 02, Part 2: " <> s) (part2' xs @?= x)