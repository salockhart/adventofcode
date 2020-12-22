module Day18Spec (spec) where

import Day18 (part1, part2)
import Test.Hspec

input1 = "1 + 2 * 3 + 4 * 5 + 6"

input2 = "1 + (2 * 3) + (4 * (5 + 6))"

input3 = "2 * 3 + (4 * 5)"

input4 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"

input5 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"

input6 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 71 for " ++ input1) $ do
      part1 input1 `shouldBe` "71"
    it ("returns 51 for " ++ input2) $ do
      part1 input2 `shouldBe` "51"
    it ("returns 26 for " ++ input3) $ do
      part1 input3 `shouldBe` "26"
    it ("returns 437 for " ++ input4) $ do
      part1 input4 `shouldBe` "437"
    it ("returns 12240 for " ++ input5) $ do
      part1 input5 `shouldBe` "12240"
    it ("returns 13632 for " ++ input6) $ do
      part1 input6 `shouldBe` "13632"

  describe "Part2" $ do
    it ("returns 231 for " ++ input1) $ do
      part2 input1 `shouldBe` "231"
    it ("returns 51 for " ++ input2) $ do
      part2 input2 `shouldBe` "51"
    it ("returns 46 for " ++ input3) $ do
      part2 input3 `shouldBe` "46"
    it ("returns 1445 for " ++ input4) $ do
      part2 input4 `shouldBe` "1445"
    it ("returns 669060 for " ++ input5) $ do
      part2 input5 `shouldBe` "669060"
    it ("returns 23340 for " ++ input6) $ do
      part2 input6 `shouldBe` "23340"
