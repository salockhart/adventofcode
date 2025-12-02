module Day02 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.String (splitOn)
import AOC.Data.Tuple (fromList2)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2025 02 part1 part2

parse :: T.Text -> [(Int, Int)]
parse =
  map
    ( fromList2
        . map (read :: String -> Int)
        . splitOn "-"
    )
    . splitOn ","
    . T.unpack

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n cs = let (c, rest) = splitAt n cs in c : chunks n rest

solve :: (String -> [[[Char]]]) -> T.Text -> Int
solve f =
  sum
    . map ((read :: String -> Int) . concat . head)
    . filter (any (\xs -> all (== head xs) xs))
    . map (f . show)
    . concatMap (\(a, b) -> [a .. b])
    . parse

part1 :: T.Text -> Int
part1 = solve ((: []) . (\(a, b) -> [a, b]) . (\x -> splitAt (length x `div` 2) x))

part2 :: T.Text -> Int
part2 = solve (\x -> map (`chunks` x) [1 .. (length x `div` 2)])
