module Day06Spec (spec) where

import Day06 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 1000000 for turn on 0,0 through 999,999" $ do
      part1 "turn on 0,0 through 999,999" `shouldBe` 1000000
    it "returns 1000 for toggle 0,0 through 999,0" $ do
      part1 "toggle 0,0 through 999,0" `shouldBe` 1000
    it "returns 0 for turn off 499,499 through 500,500" $ do
      part1 "turn off 499,499 through 500,500" `shouldBe` 0
    it "returns 0 for turn on 0,0 through 9,9\nturn off 0,0 through 9,9" $ do
      part1 "turn on 0,0 through 9,9\nturn off 0,0 through 9,9" `shouldBe` 0

  describe "Part2" $ do
    it "returns 1 for turn on 0,0 through 0,0" $ do
      part2 "turn on 0,0 through 0,0" `shouldBe` 1
    it "returns 2000000 for turn on 0,0 through 0,0" $ do
      part2 "toggle 0,0 through 999,999" `shouldBe` 2000000
