module Day03Spec (spec) where

import qualified Data.Text as T
import Day03 (part1, part2)
import Test.Hspec

input :: T.Text
input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

input2 :: T.Text
input2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 161)
    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 161)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 161)
    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 48)
