{-# LANGUAGE OverloadedStrings #-}

module Day10 (main, part1, part2) where

import AOC (forceUnwrap, mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, readCoordMap)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2023 10 part1 part2

validNeighbours :: ((Int, Int), Char) -> [((Int, Int), [Char])]
validNeighbours ((x, y), 'S') =
  [ ((x, y - 1), ['|', '7', 'F']),
    ((x, y + 1), ['|', 'L', 'J']),
    ((x - 1, y), ['-', 'L', 'F']),
    ((x + 1, y), ['-', 'J', '7'])
  ]
validNeighbours ((x, y), '|') =
  [ ((x, y - 1), ['|', '7', 'F']),
    ((x, y + 1), ['|', 'L', 'J'])
  ]
validNeighbours ((x, y), '-') =
  [ ((x - 1, y), ['-', 'L', 'F']),
    ((x + 1, y), ['-', 'J', '7'])
  ]
validNeighbours ((x, y), 'L') =
  [ ((x, y - 1), ['|', '7', 'F']),
    ((x + 1, y), ['-', 'J', '7'])
  ]
validNeighbours ((x, y), 'J') =
  [ ((x, y - 1), ['|', '7', 'F']),
    ((x - 1, y), ['-', 'L', 'F'])
  ]
validNeighbours ((x, y), '7') =
  [ ((x, y + 1), ['|', 'L', 'J']),
    ((x - 1, y), ['-', 'L', 'F'])
  ]
validNeighbours ((x, y), 'F') =
  [ ((x, y + 1), ['|', 'L', 'J']),
    ((x + 1, y), ['-', 'J', '7'])
  ]
validNeighbours _ = error "bad"

findLoop :: CoordMap Char -> [(Coord, Char)]
findLoop coordMap = do
  let startPosition = forceUnwrap $ find ((== 'S') . snd) $ M.toList coordMap
  findLoop' coordMap startPosition

findLoop' :: CoordMap Char -> (Coord, Char) -> [(Coord, Char)]
findLoop' coordMap pos =
  let nextPlaces = getNeighbours pos
   in if null nextPlaces
        then [pos]
        else
          let nextPlace = head nextPlaces
              nextMap = M.delete (fst nextPlace) coordMap
           in pos : findLoop' nextMap nextPlace
  where
    getNeighbours = mapMaybe (uncurry maybeAllowed) . validNeighbours
    maybeAllowed coord allowedNeighbours =
      coordMap M.!? coord
        >>= ( \v ->
                if v `elem` allowedNeighbours
                  then Just (coord, v)
                  else Nothing
            )

-- https://en.wikipedia.org/wiki/Shoelace_formula
shoelace :: [(Int, Int)] -> Int
shoelace = abs . (`div` 2) . sum . shoelace' . bookend
  where
    bookend xs = last xs : xs
    shoelace' ((x0, y0) : p1@(x1, y1) : rest) =
      ((y0 + y1) * (x0 - x1)) : shoelace' (p1 : rest)
    shoelace' _ = []

-- https://en.wikipedia.org/wiki/Pick%27s_theorem
numInteriorPoints :: [Coord] -> Int -> Int
numInteriorPoints path area = area + 1 - (length path `div` 2)

part1 :: T.Text -> Int
part1 =
  (`div` 2)
    . length
    . findLoop
    . readCoordMap
    . lines
    . T.unpack

part2 :: T.Text -> Int
part2 =
  (\path -> numInteriorPoints path (shoelace path))
    . map fst
    . findLoop
    . readCoordMap
    . lines
    . T.unpack
