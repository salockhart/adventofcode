module Day22Spec (spec) where

import qualified Data.Text as T
import Day22 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "1\n\
  \10\n\
  \100\n\
  \2024"

input2 :: T.Text
input2 =
  "1\n\
  \2\n\
  \3\n\
  \2024"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 37327623)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 23)
