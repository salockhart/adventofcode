module Day06Spec (spec) where

import Day06 (part1, part2)
import Test.Hspec

input =
  "abc\n\
  \\n\
  \a\n\
  \b\n\
  \c\n\
  \\n\
  \ab\n\
  \ac\n\
  \\n\
  \a\n\
  \a\n\
  \a\n\
  \a\n\
  \\n\
  \b"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 11 for " ++ input) $ do
      part1 input `shouldBe` 11

  describe "Part2" $ do
    it ("returns 6 for " ++ input) $ do
      part2 input `shouldBe` 6
