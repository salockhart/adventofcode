module Day13Spec (spec) where

import Day13 (part1, part2)
import Test.Hspec

input1 =
  "939\n\
  \7,13,x,x,59,x,31,19"

input2 =
  "0\n\
  \17,x,13,19"

input3 =
  "0\n\
  \67,7,59,61"

input4 =
  "0\n\
  \67,x,7,59,61"

input5 =
  "0\n\
  \67,7,x,59,61"

input6 =
  "0\n\
  \1789,37,47,1889"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 295 for " ++ input1) $ do
      part1 input1 `shouldBe` "295"

  describe "Part2" $ do
    it ("returns 1068781 for " ++ input1) $ do
      part2 input1 `shouldBe` "1068781"
    it ("returns 3417 for " ++ input2) $ do
      part2 input2 `shouldBe` "3417"
    it ("returns 754018 for " ++ input3) $ do
      part2 input3 `shouldBe` "754018"
    it ("returns 779210 for " ++ input4) $ do
      part2 input4 `shouldBe` "779210"
    it ("returns 1261476 for " ++ input5) $ do
      part2 input5 `shouldBe` "1261476"
    it ("returns 1202161486 for " ++ input6) $ do
      part2 input6 `shouldBe` "1202161486"
