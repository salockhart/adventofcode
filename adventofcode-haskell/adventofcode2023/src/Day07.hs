module Day07 (main, part1, part2) where

import AOC (mkAoCMain)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.List (find, group, partition, sort, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2023 07 part1 part2

parse :: T.Text -> [(String, Int)]
parse = map (parseRow . T.splitOn " ") . T.lines
  where
    parseRow [hand, bid] = (T.unpack hand, read (T.unpack bid) :: Int)
    parseRow _ = error "failed to parse row"

compareHands :: (String -> Int) -> (Char -> Int) -> (String, Int) -> (String, Int) -> Ordering
compareHands rank converter (hand1, _) (hand2, _)
  | rank1 < rank2 = LT
  | rank1 > rank2 = GT
  | otherwise =
      uncurry compare $
        bimap converter converter $
          fromJust $
            find (uncurry (/=)) $
              zip hand1 hand2
  where
    rank1 = rank hand1
    rank2 = rank hand2

convertCard :: Char -> Int
convertCard 'T' = 10
convertCard 'J' = 11
convertCard 'Q' = 12
convertCard 'K' = 13
convertCard 'A' = 14
convertCard x = read [x]

rank' :: [Int] -> Int
rank' [5] = 6 -- five of a kind
rank' [1, 4] = 5 -- four of a kind
rank' [2, 3] = 4 -- full house
rank' [1, 1, 3] = 3 -- three of a kind
rank' [1, 2, 2] = 2 -- two pair
rank' [1, 1, 1, 2] = 1 -- one pair
rank' [1, 1, 1, 1, 1] = 0 -- high card
rank' _ = error "invalid hand"

part1 :: T.Text -> Int
part1 =
  sum
    . zipWith (*) [1 ..]
    . map snd
    . sortBy (compareHands rank convertCard)
    . parse
  where
    rank = rank' . sort . map length . group . sort

part2 :: T.Text -> Int
part2 =
  sum
    . zipWith (*) [1 ..]
    . map snd
    . sortBy (compareHands rank convertCard')
    . parse
  where
    -- J is worth 1 now, not 11
    convertCard' = (\n -> if n == 11 then 1 else n) . convertCard
    rank =
      rank'
        . ( \(ns, js) ->
              if not $ null ns -- handle JJJJJ as a hand
                then init ns ++ [last ns + length js]
                else [5]
          )
        . first (sort . map length . group . sort)
        . partition (/= 'J')
