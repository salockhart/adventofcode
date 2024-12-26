module Day07 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.Tuple (fromList2)
import Data.Bifunctor (Bifunctor (second), bimap, second)
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 07 part1 part2

parse :: T.Text -> [(Int, [Int])]
parse =
  map
    ( bimap
        (read . T.unpack)
        ( map (read . T.unpack)
            . T.splitOn " "
        )
        . fromList2
        . T.splitOn ": "
    )
    . T.lines

options :: [t -> t -> t] -> [t] -> [t]
options _ [] = []
options _ [a] = [a]
options ops (a : rest) = do
  let rs = options ops rest
  concatMap (\r -> map (\op -> op a r) ops) rs

solve :: [Int -> Int -> Int] -> T.Text -> Int
solve ops =
  sum
    . map fst
    . filter
      ( uncurry S.member
          . second
            ( S.fromList
                . options ops
                . reverse
            )
      )
    . parse

part1 :: T.Text -> Int
part1 = solve [(+), (*)]

part2 :: T.Text -> Int
part2 = solve [(+), (*), \a b -> read (show b ++ show a)]
