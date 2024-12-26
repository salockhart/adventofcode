module Day05 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.Tuple (fromList2)
import Data.Bifunctor (bimap)
import Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 05 part1 part2

parse :: T.Text -> (M.Map (Int, Int) Ordering, [[Int]])
parse =
  bimap
    ( M.fromList
        . concatMap ((\t@((a, b), _) -> [t, ((b, a), GT)]) . (,LT) . fromList2)
        . parseSection "|"
    )
    (parseSection ",")
    . fromList2
    . T.splitOn "\n\n"
    . T.strip
  where
    parseSection divider =
      map
        ( map (read . T.unpack)
            . T.splitOn divider
        )
        . T.splitOn "\n"

solve :: ([Int] -> [Int] -> Bool) -> (M.Map (Int, Int) Ordering, [[Int]]) -> Int
solve withOrdered =
  sum
    . map middle
    . ( \(key, us) ->
          map fst $
            filter (uncurry withOrdered) $
              map (\u -> (sort key u, u)) us
      )
  where
    sort key = sortBy (\a b -> M.findWithDefault EQ (a, b) key)
    middle xs = xs !! (length xs `div` 2)

part1 :: T.Text -> Int
part1 = solve (==) . parse

part2 :: T.Text -> Int
part2 = solve (/=) . parse
