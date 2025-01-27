module Day23Spec (spec) where

import qualified Data.Text as T
import Day23 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "kh-tc\n\
  \qp-kh\n\
  \de-cg\n\
  \ka-co\n\
  \yn-aq\n\
  \qp-ub\n\
  \cg-tb\n\
  \vc-aq\n\
  \tb-ka\n\
  \wh-tc\n\
  \yn-cg\n\
  \kh-ub\n\
  \ta-co\n\
  \de-co\n\
  \tc-td\n\
  \tb-wq\n\
  \wh-td\n\
  \ta-ka\n\
  \td-qp\n\
  \aq-cg\n\
  \wq-ub\n\
  \ub-vc\n\
  \de-ta\n\
  \wq-aq\n\
  \wq-vc\n\
  \wh-yn\n\
  \ka-de\n\
  \kh-ta\n\
  \co-tc\n\
  \wh-qp\n\
  \tb-vc\n\
  \td-yn"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 7)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== "co,de,ka,ta")
