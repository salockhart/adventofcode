module Day10Spec (spec) where

import Day10 (part1, part2)
import Test.Hspec

input =
  "[({(<(())[]>[[{[]{<()<>>\n\
  \[(()[<>])]({[<{<<[]>>(\n\
  \{([(<{}[<>[]}>{[]{[(<()>\n\
  \(((({<>}<{<{<>}{[]{[]{}\n\
  \[[<[([]))<([[{}[[()]]]\n\
  \[{[{({}]{}}([{[{{{}}([]\n\
  \{<[[]]>}<{[{[{[]{()[[[]\n\
  \[<(<(<(<{}))><([]([]()\n\
  \<{([([[(<>()){}]>(<<{{\n\
  \<{([{{}}[<[[[<>{}]]]>[]]"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "26397"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "288957"
