module Day01Spec (spec) where

import qualified Data.Text as T
import Day01 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "L68\n\
  \L30\n\
  \R48\n\
  \L5\n\
  \R60\n\
  \L55\n\
  \L1\n\
  \L99\n\
  \R14\n\
  \L82"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 3)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 6)
