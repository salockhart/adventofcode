module Day01Spec (spec) where

import Day01 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "1122" $ do
      part1 "1122" `shouldBe` "3"
    it "1111" $ do
      part1 "1111" `shouldBe` "4"
    it "1234" $ do
      part1 "1234" `shouldBe` "0"
    it "91212129" $ do
      part1 "91212129" `shouldBe` "9"

  describe "Part2" $ do
    it "1212" $ do
      part2 "1212" `shouldBe` "6"
    it "1221" $ do
      part2 "1221" `shouldBe` "0"
    it "123425" $ do
      part2 "123425" `shouldBe` "4"
    it "123123" $ do
      part2 "123123" `shouldBe` "12"
    it "12131415" $ do
      part2 "12131415" `shouldBe` "4"
