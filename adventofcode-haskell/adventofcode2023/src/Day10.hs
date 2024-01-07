module Day10 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, picksTheorem, readCoordMap)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
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
  let startPosition = fromJust $ find ((== 'S') . snd) $ M.toList coordMap
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
  picksTheorem
    . map fst
    . findLoop
    . readCoordMap
    . lines
    . T.unpack
