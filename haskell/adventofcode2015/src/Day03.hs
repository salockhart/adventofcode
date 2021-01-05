module Day03 (main, part1, part2) where

import qualified Data.Set as Set

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x : y : xys) = (x : xs, y : ys) where (xs, ys) = split xys

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (la, lb) (ra, rb) = (la + ra, lb + rb)

translation :: Char -> (Int, Int)
translation '^' = (0, 1)
translation 'v' = (0, -1)
translation '>' = (1, 0)
translation '<' = (-1, 0)

walk :: [(Int, Int)] -> String -> [(Int, Int)]
walk coords [] = coords
walk (first : coords) (char : future) = walk ((translation char `plus` first) : first : coords) future

part1 :: String -> Int
part1 = length . Set.fromList . walk [(0, 0)]

part2 :: String -> Int
part2 input = do
  let splitInput = split input
  let santa = walk [(0, 0)] (fst splitInput)
  let robo = walk [(0, 0)] (snd splitInput)
  length (Set.fromList (santa ++ robo))
