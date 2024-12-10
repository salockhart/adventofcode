module Day10Spec (spec) where

import qualified Data.Text as T
import Day10 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "0123\n\
  \1234\n\
  \8765\n\
  \9876"

input2 :: T.Text
input2 =
  "89010123\n\
  \78121874\n\
  \87430965\n\
  \96549874\n\
  \45678903\n\
  \32019012\n\
  \01329801\n\
  \10456732"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 1)
    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 36)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 81)
