module Day06 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.String (splitOn, strip)
import Data.List (transpose)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2025 06 part1 part2

parseHorizontal :: T.Text -> [(Char, [Int])]
parseHorizontal =
  map (equations . reverse) -- 4. operator is in the last number, so flip and build the equations
    . transpose -- 3. then transpose, so we read the numbers from top to bottom in each column
    . map (filter (/= "") . splitOn " ") -- 2. split each line up by spaces
    . lines -- 1. grab all the lines
    . T.unpack
  where
    equations (a : rest) = (head a, map (read :: String -> Int) rest)
    equations _ = error "cannot parse, bad format"

parseVertical :: T.Text -> [(Char, [Int])]
parseVertical =
  equations -- 5. build the equations
    . filter (/= "") -- 4. remove empties
    . map strip -- 3. tidy up the lines
    . reverse -- 2. the transpose will put the columns left to right, we want right to left
    . transpose -- 1. this is key, flipping the columns into lines
    . lines
    . T.unpack
  where
    -- we have a list of all the figures here
    -- this breaks up the lists into lists of equations, using the operator to know where to break
    -- `break` will only include the ones that match, though, so we pop the last number off the stack and include that
    equations [] = []
    equations xs = case break (\x -> let l = last x in l `elem` ['+', '*']) xs of
      (nums, op : rest) -> (last op, map (read :: String -> Int) (nums ++ [init op])) : equations rest
      _ -> error "unexpected pattern"

solve :: Char -> [Int] -> Int
solve '+' xs = sum xs
solve '*' xs = product xs
solve _ _ = error "unknown operator"

part1 :: T.Text -> Int
part1 =
  sum
    . map (uncurry solve)
    . parseHorizontal

part2 :: T.Text -> Int
part2 =
  sum
    . map (uncurry solve)
    . parseVertical
