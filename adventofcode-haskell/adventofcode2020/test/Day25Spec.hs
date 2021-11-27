module Day25Spec (spec) where

import Day25 (part1)
import Test.Hspec

input =
  "5764801\n\
  \17807724"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 14897079 for " ++ input) $ do
      part1 input `shouldBe` "14897079"
