module Day05Spec (spec) where

import Day05 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 1 for ugknbfddgicrmopn" $ do
      part1 "ugknbfddgicrmopn" `shouldBe` 1
    it "returns 1 for aaa" $ do
      part1 "aaa" `shouldBe` 1
    it "returns 0 for jchzalrnumimnmhp" $ do
      part1 "jchzalrnumimnmhp" `shouldBe` 0
    it "returns 1 for haegwjzuvuyypxyu" $ do
      part1 "haegwjzuvuyypxyu" `shouldBe` 0
    it "returns 1 for dvszwmarrgswjxmb" $ do
      part1 "dvszwmarrgswjxmb" `shouldBe` 0

  describe "Part2" $ do
    it "returns 1 for qjhvhtzxzqqjkmpb" $ do
      part2 "qjhvhtzxzqqjkmpb" `shouldBe` 1
    it "returns 1 for xxyxx" $ do
      part2 "xxyxx" `shouldBe` 1
    it "returns 0 for uurcxstgmygtbstg" $ do
      part2 "uurcxstgmygtbstg" `shouldBe` 0
    it "returns 0 for ieodomkazucvgmuy" $ do
      part2 "ieodomkazucvgmuy" `shouldBe` 0
