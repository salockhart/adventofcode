module Day02 (main, part1, part2) where

import Data.List
import qualified Data.Text as T

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

areas :: [Int] -> [Int]
areas [l, w, h] = [l * w, w * h, h * l]

volume :: [Int] -> Int
volume = product

ribbon :: [Int] -> Int
ribbon = (2 *) . sum . init . sort

paper :: [Int] -> Int
paper a = sum (minimum a : map (* 2) a)

parse :: String -> [[Int]]
parse = map (map (read . T.unpack) . T.split (== 'x') . T.pack) . lines

part1 :: String -> Int
part1 = sum . map (paper . areas) . parse

part2 :: String -> Int
part2 = sum . map (\l -> ribbon l + volume l) . parse
