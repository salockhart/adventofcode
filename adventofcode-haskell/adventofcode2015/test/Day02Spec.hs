module Day02Spec (spec) where

import Day02 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 58 for 2x3x4" $ do
      part1 "2x3x4" `shouldBe` 58

    it "returns 58 for 1x1x10" $ do
      part1 "1x1x10" `shouldBe` 43

  describe "Part2" $ do
    it "returns 34 for 2x3x4" $ do
      part2 "2x3x4" `shouldBe` 34

    it "returns 14 for 1x1x10" $ do
      part2 "1x1x10" `shouldBe` 14
