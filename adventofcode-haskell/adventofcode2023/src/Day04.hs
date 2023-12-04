{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day04 (main, part1, part2) where

import AOC (groupOn, solveAoCDay)
import Data.List (sortOn)
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = solveAoCDay 2023 04 part1 part2

-- Card n: {winning numbers} | {chosen numbers}
parse :: T.Text -> [S.Set T.Text]
parse =
  map (S.fromList . filter (not . T.null) . T.splitOn " ")
    . T.splitOn " | "
    . (!! 1)
    . T.splitOn ": "

getReplacements :: [T.Text] -> [[Int]]
getReplacements =
  -- each matching number means an additional card copied from
  -- those that follow
  zipWith
    (\i numWins -> take numWins (drop i [1 :: Int ..]))
    [1 ..]
    -- get the number of correct picks
    . map (length . intersection' . parse)
  where
    intersection' [winningNumbers, numbersIHave] = S.intersection winningNumbers numbersIHave
    intersection' _ = error "something went wrong"

part1 :: T.Text -> Int
part1 =
  sum
    -- the winnings are calculated by the number of winning numbers
    . map ((\l -> if l == 0 then 0 :: Int else 2 ^ (l - 1)) . length)
    . getReplacements
    . T.lines

part2 :: T.Text -> Integer
part2 =
  solve
    . getReplacements
    . T.lines
  where
    solve replacements =
      fst $
        -- until we have exhausted the deck...
        until
          (null . snd)
          -- copy the cards
          copyCards
          -- starting with 0 tallied, and one of each card
          (0, map (,1) [1 .. (length replacements)])
      where
        -- to copy the cards...
        copyCards (total, (i, count) : rest) =
          -- find out what we are replacing it with
          let replaceWith = replacements !! (i - 1)
           in ( -- tally our cards
                total + count,
                -- and then add in the additional cards to the deck
                map (\xs -> (fst $ head xs, sum $ map snd xs)) $
                  groupOn fst $
                    sortOn fst $
                      rest ++ map (,count) replaceWith
              )
        copyCards _ = error "oops"
