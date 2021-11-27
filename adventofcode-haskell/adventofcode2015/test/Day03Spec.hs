module Day03Spec (spec) where

import Day03 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 2 for >" $ do
      part1 ">" `shouldBe` 2

    it "returns 4 for ^>v<" $ do
      part1 "^>v<" `shouldBe` 4

    it "returns 2 for ^v^v^v^v^v" $ do
      part1 "^v^v^v^v^v" `shouldBe` 2

  describe "Part2" $ do
    it "returns 3 for ^v" $ do
      part2 "^v" `shouldBe` 3

    it "returns 3 for ^>v<" $ do
      part2 "^>v<" `shouldBe` 3

    it "returns 11 for ^v^v^v^v^v" $ do
      part2 "^v^v^v^v^v" `shouldBe` 11
