module Day01Spec (spec) where

import Day01 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 0 for (())" $ do
      part1 "(())" `shouldBe` (0 :: Int)

    it "returns 0 for ()()" $ do
      part1 "()()" `shouldBe` (0 :: Int)

    it "returns 3 for (((" $ do
      part1 "(((" `shouldBe` (3 :: Int)

    it "returns 3 for (()(()(" $ do
      part1 "(()(()(" `shouldBe` (3 :: Int)

    it "returns 3 for ))(((((" $ do
      part1 "))(((((" `shouldBe` (3 :: Int)

    it "returns -1 for ())" $ do
      part1 "())" `shouldBe` (-1 :: Int)

    it "returns -1 for ))(" $ do
      part1 "))(" `shouldBe` (-1 :: Int)

    it "returns -3 for )))" $ do
      part1 ")))" `shouldBe` (-3 :: Int)

    it "returns -1 for )())())" $ do
      part1 ")())())" `shouldBe` (-3 :: Int)

  describe "Part2" $ do
    it "returns 1 for )" $ do
      part2 ")" `shouldBe` (1 :: Int)

    it "returns 5 for ()())" $ do
      part2 "()())" `shouldBe` (5 :: Int)