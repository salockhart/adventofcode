module Day18 (main, part1, part2) where

import Data.List (findIndex)
import Data.Maybe (isJust)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: [Char] -> [(Int, Int)]
parse = reverse . snd . foldl parse' (0, [])
  where
    parse' (i, t) '[' = (i + 1, t)
    parse' (i, t) ']' = (i - 1, t)
    parse' (i, t) ',' = (i, t)
    parse' (i, t) x = (i, (read [x], i) : t)

normalizeNumbers :: [(Int, Int)] -> [(Int, Int)]
normalizeNumbers t = do
  let indexOfExplosion = findIndex (\(x, l) -> l == 5) t
  let indexOfSplit = findIndex (\(x, l) -> x > 9) t

  if isJust indexOfExplosion
    then normalizeNumbers $ maybe t (applyExplosionAt t) indexOfExplosion
    else
      if isJust indexOfSplit
        then normalizeNumbers $ maybe t (applySplitAt t) indexOfSplit
        else t
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
    applySplitAt t i = do
      let (before, (x, l) : after) = splitAt i t
      let half = x `quot` 2
      before ++ [(half, l + 1), (x - half, l + 1)] ++ after

addNumbers :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
addNumbers a b =
  normalizeNumbers $
    map (\(x, l) -> (x, l + 1)) (a ++ b)

magnitudeNumber :: [(Int, Int)] -> Int
magnitudeNumber [] = error "cannot magnitude"
magnitudeNumber (t : ts) = fst $ head $ foldl handleStack [t] ts
  where
    handleStack [] t = [t]
    handleStack (h@(xh, lh) : hs) a@(xa, la) =
      if la == lh
        then handleStack hs (3 * xh + 2 * xa, lh - 1)
        else a : h : hs

part1 :: String -> String
part1 =
  show
    . magnitudeNumber
    . foldl1 addNumbers
    . map parse
    . lines

part2 :: String -> String
part2 =
  show
    . maximum
    . map (magnitudeNumber . uncurry addNumbers)
    . (\xs -> [(a, b) | a <- xs, b <- xs, a /= b])
    . map parse
    . lines
