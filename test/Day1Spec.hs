module Day1Spec (spec) where

import Day1 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 514579 for 1721,979,366,299,675,1456" $ do
      part1 "1721\n979\n366\n299\n675\n1456" `shouldBe` 514579

  describe "Part2" $ do
    it "returns 241861950 for 1721,979,366,299,675,1456" $ do
      part2 "1721\n979\n366\n299\n675\n1456" `shouldBe` 241861950
