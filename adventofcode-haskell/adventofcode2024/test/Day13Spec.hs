module Day13Spec (spec) where

import qualified Data.Text as T
import Day13 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "Button A: X+94, Y+34\n\
  \Button B: X+22, Y+67\n\
  \Prize: X=8400, Y=5400\n\
  \\n\
  \Button A: X+26, Y+66\n\
  \Button B: X+67, Y+21\n\
  \Prize: X=12748, Y=12176\n\
  \\n\
  \Button A: X+17, Y+86\n\
  \Button B: X+84, Y+37\n\
  \Prize: X=7870, Y=6450\n\
  \\n\
  \Button A: X+69, Y+23\n\
  \Button B: X+27, Y+71\n\
  \Prize: X=18641, Y=10279"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 480)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 875318608908)
