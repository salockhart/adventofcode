module Day05Spec (spec) where

import Day05 (part1)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 357 for FBFBBFFRLR" $ do
      part1 "FBFBBFFRLR" `shouldBe` 357
    it "returns 567 for BFFFBBFRRR" $ do
      part1 "BFFFBBFRRR" `shouldBe` 567
    it "returns 119 for FFFBBBFRRR" $ do
      part1 "FFFBBBFRRR" `shouldBe` 119
    it "returns 820 for BBFFBBFRLL" $ do
      part1 "BBFFBBFRLL" `shouldBe` 820
