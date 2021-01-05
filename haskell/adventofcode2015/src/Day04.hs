module Day04 (main, part1, part2) where

import Data.Hash.MD5
import Data.String.Utils

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

searchFor :: String -> String -> String
searchFor x input = filter (`elem` ['0' .. '9']) . head $ dropWhile (not . startswith x . md5s . Str) $ map ((input ++) . show) [0 ..]

part1 :: String -> String
part1 = searchFor "00000"

part2 :: String -> String
part2 = searchFor "000000"
