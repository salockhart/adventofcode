{-# LANGUAGE TupleSections #-}

module Day20 (main, part1, part2) where

import AOC (Coord, CoordMap, btoi, parseIntoCoordMap)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> ([Int], CoordMap Int)
parse s =
  let (a : img : _) = splitOn "\n\n" s
   in (map charToInt a, addPadding 0 $ parseIntoCoordMap $ map (map charToInt) $ lines img)
  where
    charToInt '.' = 0
    charToInt '#' = 1
    charToInt _ = error "cannot parse"

bounds :: CoordMap a -> (Int, Int, Int, Int)
bounds =
  ( \coords ->
      ( (minimum . map fst) coords,
        (maximum . map fst) coords,
        (minimum . map snd) coords,
        (maximum . map snd) coords
      )
  )
    . Map.keys

addPadding :: a -> CoordMap a -> CoordMap a
addPadding c m = do
  let padding = 2
  let (minX, maxX, minY, maxY) = bounds m
  let paddingCoords =
        [ (x, y)
          | x <- [(minX - padding) .. (maxX + padding)],
            y <- [(minY - padding) .. (maxY + padding)],
            not (x >= minX && x <= maxX && y >= minY && y <= maxY)
        ]
  Map.union m (Map.fromList $ map (,c) paddingCoords)

enhance :: Int -> ([Int], CoordMap Int) -> CoordMap Int
enhance 0 (_, img) = img
enhance n (a, img) =
  enhance
    (n - 1)
    (a, addPadding (unknownValue (n + 1)) $ Map.mapWithKey (\k v -> outputPixel k) img)
  where
    unknownValue n = if head a == 0 then 0 else [0, 1] !! (n `mod` 2)
    outputPixel (x, y) =
      (a !!) $
        btoi $
          [ (\c -> fromMaybe (unknownValue n) $ img Map.!? c) (x', y')
            | y' <- [y - 1 .. y + 1],
              x' <- [x - 1 .. x + 1]
          ]

part1 :: String -> String
part1 =
  show
    . length
    . Map.filter (== 1)
    . enhance 2
    . parse

part2 :: String -> String
part2 =
  show
    . length
    . Map.filter (== 1)
    . enhance 50
    . parse
