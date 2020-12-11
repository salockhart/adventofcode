module Day10Spec (spec) where

import Day10 (part1, part2)
import Test.Hspec

input1 =
  "16\n\
  \10\n\
  \15\n\
  \5\n\
  \1\n\
  \11\n\
  \7\n\
  \19\n\
  \6\n\
  \12\n\
  \4"

input2 =
  "28\n\
  \33\n\
  \18\n\
  \42\n\
  \31\n\
  \14\n\
  \46\n\
  \20\n\
  \48\n\
  \47\n\
  \24\n\
  \23\n\
  \49\n\
  \45\n\
  \19\n\
  \38\n\
  \39\n\
  \11\n\
  \1\n\
  \32\n\
  \25\n\
  \35\n\
  \8\n\
  \17\n\
  \7\n\
  \9\n\
  \4\n\
  \2\n\
  \34\n\
  \10\n\
  \3"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 35 for " ++ input1) $ do
      part1 input1 `shouldBe` 35

    it ("returns 220 for " ++ input2) $ do
      part1 input2 `shouldBe` 220

  describe "Part2" $ do
    it ("returns 8 for " ++ input1) $ do
      part2 input1 `shouldBe` "8"

    it ("returns 19208 for " ++ input2) $ do
      part2 input2 `shouldBe` "19208"
