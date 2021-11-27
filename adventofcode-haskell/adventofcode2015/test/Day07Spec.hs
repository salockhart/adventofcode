module Day07Spec (spec) where

import Day07 (part1, part2)
import Test.Hspec

input =
  "123 -> x\n\
  \456 -> y\n\
  \x AND y -> d\n\
  \x OR y -> e\n\
  \x LSHIFT 2 -> f\n\
  \y RSHIFT 2 -> g\n\
  \NOT x -> h\n\
  \NOT y -> i"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 72 for d for " ++ input) $ do
      part1 "d" input `shouldBe` 72
    it ("returns 507 for e for " ++ input) $ do
      part1 "e" input `shouldBe` 507
    it ("returns 492 for f for " ++ input) $ do
      part1 "f" input `shouldBe` 492
    it ("returns 114 for g for " ++ input) $ do
      part1 "g" input `shouldBe` 114
    it ("returns 65412 for h for " ++ input) $ do
      part1 "h" input `shouldBe` 65412
    it ("returns 65079 for i for " ++ input) $ do
      part1 "i" input `shouldBe` 65079
    it ("returns 123 for x for " ++ input) $ do
      part1 "x" input `shouldBe` 123
    it ("returns 456 for y for " ++ input) $ do
      part1 "y" input `shouldBe` 456
