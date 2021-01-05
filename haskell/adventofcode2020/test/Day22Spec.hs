module Day22Spec (spec) where

import Day22 (part1, part2)
import Test.Hspec

input =
  "Player 1:\n\
  \9\n\
  \2\n\
  \6\n\
  \3\n\
  \1\n\
  \\n\
  \Player 2:\n\
  \5\n\
  \8\n\
  \4\n\
  \7\n\
  \10"

input2 =
  "Player 1:\n\
  \43\n\
  \19\n\
  \\n\
  \Player 2:\n\
  \2\n\
  \29\n\
  \14"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 306 for " ++ input) $ do
      part1 input `shouldBe` "306"

  describe "Part2" $ do
    it ("returns 291 for " ++ input) $ do
      part2 input `shouldBe` "291"
    it ("returns 105 for " ++ input2) $ do
      part2 input2 `shouldBe` "105"
