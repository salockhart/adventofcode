module Day20Spec (spec) where

import Day20 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 4 for 70" $ do
      part1 "70" `shouldBe` "4"
    it "returns 6 for 120" $ do
      part1 "120" `shouldBe` "6"

  describe "Part2" $ do
    it "returns 4 for 70" $ do
      part2 "70" `shouldBe` "4"
    it "returns 6 for 120" $ do
      part2 "120" `shouldBe` "6"
