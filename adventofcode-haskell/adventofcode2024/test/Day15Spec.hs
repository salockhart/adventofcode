module Day15Spec (spec) where

import qualified Data.Text as T
import Day15 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "########\n\
  \#..O.O.#\n\
  \##@.O..#\n\
  \#...O..#\n\
  \#.#.O..#\n\
  \#...O..#\n\
  \#......#\n\
  \########\n\
  \\n\
  \<^^>>>vv<v>>v<<\n\
  \"

input2 :: T.Text
input2 =
  "##########\n\
  \#..O..O.O#\n\
  \#......O.#\n\
  \#.OO..O.O#\n\
  \#..O@..O.#\n\
  \#O#..O...#\n\
  \#O..O..O.#\n\
  \#.OO.O.OO#\n\
  \#....O...#\n\
  \##########\n\
  \\n\
  \<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
  \vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
  \><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
  \<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
  \^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
  \^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
  \>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
  \<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
  \^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
  \v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

input3 :: T.Text
input3 =
  "#######\n\
  \#...#.#\n\
  \#.....#\n\
  \#..OO@#\n\
  \#..O..#\n\
  \#.....#\n\
  \#######\n\
  \\n\
  \<vv<<^^<<^^"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 2028)

    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 10092)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 9021)

    it ("\n" ++ T.unpack input3) $ do
      part2 input3 `shouldSatisfy` (== 618)
