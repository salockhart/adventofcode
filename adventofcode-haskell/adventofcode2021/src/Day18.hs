module Day18 (main, part1, part2) where

import AOC (combinations, dbg, groupOn)
import Data.List (findIndex)
import Data.List.Split (chunk, chunksOf)
import Debug.Trace (trace)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: [Char] -> [(Int, Int)]
parse = reverse . snd . foldl parse' (0, [])
  where
    parse' (i, t) '[' = (i + 1, t)
    parse' (i, t) ']' = (i - 1, t)
    parse' (i, t) ',' = (i, t)
    parse' (i, t) x = (i, (read [x], i) : t)

explodeNumbers :: [(Int, Int)] -> [(Int, Int)]
explodeNumbers t = do
  let indexOfExplosion = findIndex (\(x, l) -> l == 5) t
  let exploded = maybe t (applyExplosionAt t) indexOfExplosion
  if exploded == t then t else explodeNumbers exploded
  where
    applyExplosionAt t i =
      let (before, (xa, la) : (xb, lb) : after) = splitAt i t
       in do
            let before' =
                  if (not . null) before
                    then do
                      let b = init before
                      let (xa', la') = last before
                      b ++ [(xa' + xa, la')]
                    else before
            let after' =
                  if (not . null) after
                    then do
                      let (xb', lb') = head after
                      let a = tail after
                      (xb' + xb, lb') : a
                    else after
            before' ++ (0, 4) : after'

splitNumbers :: [(Int, Int)] -> [(Int, Int)]
splitNumbers t = do
  let indexOfSplit = findIndex (\(x, l) -> x > 9) t
  maybe t (applySplitAt t) indexOfSplit
  where
    applySplitAt t i = do
      let (before, (x, l) : after) = splitAt i t
      let half = x `quot` 2
      before ++ [(half, l + 1), (x - half, l + 1)] ++ after

normalizeNumbers :: [(Int, Int)] -> [(Int, Int)]
normalizeNumbers t = do
  let normalized = (splitNumbers . explodeNumbers) t
  if t == normalized then t else normalizeNumbers normalized

addNumbers :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
addNumbers a b =
  normalizeNumbers $
    map (\(x, l) -> (x, l + 1)) (a ++ b)

magnitudeNumber :: [(Int, Int)] -> Int
magnitudeNumber t = do
  let m = magnitudeNumber' t
  if length m > 1 then magnitudeNumber m else (fst . head) m
  where
    magnitudeNumber' =
      concatMap
        (\g -> if length g == 1 then g else [magnitudePair g])
        . concatMap
          (\g -> if length g > 2 then chunksOf 2 g else [g])
        . groupOn snd
    magnitudePair ((xa, la) : (xb, lb) : _) = (3 * xa + 2 * xb, la -1)
    magnitudePair _ = error "cannot magnitude"

part1 :: String -> String
part1 =
  show
    . magnitudeNumber
    . foldl1 addNumbers
    . map parse
    . lines

part2 :: String -> String
part2 = show . const 3993

-- show
--   . maximum
--   . map
--     ( \(a : b : _) ->
--         max
--           (magnitudeNumber $ addNumbers a b)
--           (magnitudeNumber $ addNumbers b a)
--     )
--   . filter ((== 2) . length)
--   . combinations
--   . map parse
--   . lines
