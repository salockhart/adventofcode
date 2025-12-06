module Day06Spec (spec) where

import qualified Data.Text as T
import Day06 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "123 328  51 64 \n\
  \ 45 64  387 23 \n\
  \  6 98  215 314\n\
  \*   +   *   +  "

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 4277556)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 3263827)
