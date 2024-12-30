module Day19 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.Tuple (fromList2)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Function.Memoize (memoize2)
import Data.List (sortOn, stripPrefix)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 19 part1 part2

parse :: T.Text -> ([String], [String])
parse =
  bimap
    (sortOn length . map T.unpack . T.splitOn ", ")
    (lines . T.unpack)
    . fromList2
    . T.splitOn "\n\n"
    . T.strip

splitPrefix :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
splitPrefix a b = do
  rest <- stripPrefix a b
  return (a, rest)

variations :: [String] -> String -> Int
variations = memoize2 variations'
  where
    variations' _ [] = 1
    variations' ps design = do
      let xs = mapMaybe (`splitPrefix` design) ps
      sum $ map (variations ps . snd) xs

part1 :: T.Text -> Int
part1 =
  length
    . filter (> 0)
    . (\(ps, ts) -> map (variations ps) ts)
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . (\(ps, ts) -> map (variations ps) ts)
    . parse
