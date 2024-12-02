module Day02 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.List (first2, windows)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 02 part1 part2

parse :: T.Text -> [[Int]]
parse =
  map
    ( map ((read :: String -> Int) . T.unpack)
        . T.splitOn " "
    )
    . T.lines

isSafe :: [Int] -> Bool
isSafe ls = isSafe' (length ls) ls
  where
    isSafe' numLevels =
      (<= 0)
        . (numLevels -)
        . length
        . foldl handle []
    handle [] x = [x]
    handle (h : rest) x =
      if isSafeStep (x - h) && allSameDirection (x : h : rest)
        then x : h : rest
        else h : rest
    isSafeStep x = let x' = abs x in (x' >= 1 && x' <= 3)
    allSameDirection =
      (\x' -> all (>= 0) x' || all (<= 0) x')
        . map (uncurry (-) . first2)
        . windows 2
        . reverse

subOneLists :: [a] -> [[a]]
subOneLists [] = []
subOneLists (x : xs) = xs : map (x :) (subOneLists xs)

part1 :: T.Text -> Int
part1 = length . filter isSafe . parse

part2 :: T.Text -> Int
part2 =
  length
    . filter (any isSafe . (\ls -> ls : subOneLists ls))
    . parse
