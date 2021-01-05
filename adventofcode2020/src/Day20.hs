module Day20 (main, part1, part2, tileToInts) where

import AOC (groupOn, slice, splitOn, uniques)
import Data.List (groupBy, sort, sortBy, sortOn, transpose)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Ord

type Grid = Map.Map Coord Tile

type Coord = (Int, Int)

data Orientation = Ori Bool Int deriving (Show, Eq, Read, Ord)

data Tile = Tile
  { tileId :: Int,
    contents :: [[Int]]
  }
  deriving (Show, Eq, Read, Ord)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

tileToInts :: [[Char]] -> [[Int]]
tileToInts = map (map (\c -> if c == '#' then 1 else 0))

tileTop :: Tile -> [Int]
tileTop = head . contents

tileBottom :: Tile -> [Int]
tileBottom = last . contents

tileLeft :: Tile -> [Int]
tileLeft = map head . contents

tileRight :: Tile -> [Int]
tileRight = map last . contents

parse :: String -> Tile
parse str = do
  let (header : rest) = lines str
  let tileId = read $ take 4 $ drop 5 header :: Int
  Tile
    { tileId = tileId,
      contents = tileToInts rest
    }

rotate :: Int -> Tile -> Tile
rotate n tile =
  Tile
    { tileId = tileId tile,
      contents = iterate rot (contents tile) !! n
    }
  where
    rot = transpose . reverse

mirror :: Tile -> Tile
mirror tile =
  Tile
    { tileId = tileId tile,
      contents = reverse (contents tile)
    }

permutations :: Tile -> [Tile]
permutations tile = [permute flipped nrots tile | flipped <- [False, True], nrots <- [0 .. 3]]
  where
    permute :: Bool -> Int -> Tile -> Tile
    permute False n = rotate n
    permute True n = rotate n . mirror

adjacent :: Coord -> Tile -> Tile -> Maybe Coord
adjacent (x, y) a b
  | tileTop a == tileBottom b = Just (x, y - 1)
  | tileBottom a == tileTop b = Just (x, y + 1)
  | tileLeft a == tileRight b = Just (x - 1, y)
  | tileRight a == tileLeft b = Just (x + 1, y)
  | otherwise = Nothing

getNeighbours :: Coord -> [Coord]
getNeighbours (x, y) =
  [ (x - 1, y),
    (x + 1, y),
    (x, y - 1),
    (x, y + 1)
  ]

getOpenSlots :: Grid -> [Coord]
getOpenSlots grid =
  sortBy (\(ax, ay) (bx, by) -> compare (abs ax + abs ay) (abs bx + abs by)) $
    filter (isValidCoord grid) $
      uniques $
        concatMap getNeighbours $
          Map.keys grid

isValidCoord :: Grid -> Coord -> Bool
isValidCoord grid (x, y) = (x, y) `notElem` Map.keys grid

getCornerTiles :: [Tile] -> [Tile]
getCornerTiles =
  map head
    . filter ((== 4) . length)
    . groupBy (\a b -> tileId a == tileId b)
    . sortBy (\a b -> compare (tileId a) (tileId b))
    . getTilesWithUniqueTop
  where
    getTilesWithUniqueTop :: [Tile] -> [Tile]
    getTilesWithUniqueTop =
      map head
        . filter ((== 1) . length)
        . groupOn tileTop
        . sortBy (\a b -> compare (tileTop a) (tileTop b))

filterOutTileId :: Int -> [[Tile]] -> [[Tile]]
filterOutTileId tId = filter (\t -> tileId (head t) /= tId)

initialize :: [[Tile]] -> [(Grid, [[Tile]])]
initialize tiles = do
  let tiles' = concat tiles
  let cornerTiles = getCornerTiles tiles'
  map initMap $ permutations $ head cornerTiles
  where
    initMap tile = (Map.insert (0, 0) tile Map.empty, filterOutTileId (tileId tile) tiles)

buildMap :: Grid -> [[Tile]] -> [Grid]
buildMap grid [] = [grid]
buildMap grid tiles = do
  let possibleNeighbours =
        sortOn (Data.Ord.Down . length . snd)
          . filter (\(c, _) -> isValidCoord grid c)
          . filter (\(_, ns) -> not $ null ns)
          . map (\group -> (fst $ head group, concatMap snd group))
          . groupOn fst
          . map (\p -> (p, mapMaybe (\c -> grid Map.!? c >>= (\n -> Just (c, n))) (getNeighbours p)))
          $ getOpenSlots grid
  let possibles =
        [ (pCoord, t)
          | (pCoord, pNs) <- possibleNeighbours,
            t <- concat tiles,
            all (\(pNCoord, pN) -> adjacent pNCoord pN t == Just pCoord) pNs
        ]
  if null possibles
    then []
    else concatMap (uncurry buildNextMap) possibles
  where
    buildNextMap :: Coord -> Tile -> [Grid]
    buildNextMap nextCoord nextTile = do
      let grid' = Map.insert nextCoord nextTile grid
      let tiles' = filterOutTileId (tileId nextTile) tiles
      buildMap grid' tiles'

-- displayMapIds :: Grid -> String
-- displayMapIds grid = do
--   let coords = Map.keys grid
--   let xs = sort $ uniques $ map fst coords
--   let ys = sort $ uniques $ map snd coords
--   intercalate "               " [unwords [getValueOrPlaceholder grid (x, y) | y <- [minimum ys .. maximum ys]] | x <- [minimum xs .. maximum xs]] ++ "                            "
--   where
--     getValueOrPlaceholder grid coord = case grid Map.!? coord of
--       Just x -> show $ tileId x
--       Nothing -> "----"

isSquare :: Grid -> Bool
isSquare grid = do
  let coords = Map.keys grid
  let xs = map fst coords
  let ys = map snd coords
  let coords' = [(x, y) | x <- [minimum xs .. maximum xs], y <- [minimum ys .. maximum ys]]
  all (`elem` Map.keys grid) coords'

buildImage :: Grid -> [[Int]]
buildImage grid = do
  let coords = Map.keys grid
  let xs = sort $ uniques $ map fst coords
  let ys = sort $ uniques $ map snd coords
  combine grid xs ys
  where
    combine :: Grid -> [Int] -> [Int] -> [[Int]]
    combine grid xs ys = do
      let columns = [concat [trim $ contents $ grid Map.! (x, y) | y <- ys] | x <- xs]
      [concatMap (!! i) columns | i <- [0 .. (length (head columns) - 1)]]
    trim :: [[a]] -> [[a]]
    trim = tail . init . map (tail . init)

-- filterNessie :: Int -> Int -> [[Int]] -> [[[Int]]]
filterNessie img = do
  let coords = [(x, y) | y <- [0 .. (length img - rows)], x <- [0 .. (length (head img) - cols)]]
  let matches = length $ filter (\(x, y) -> matchNessie img (x, y)) coords
  length (filter (== 1) $ concat img) - (matches * 15)
  where
    rows = length nessie
    cols = length $ head nessie
    matchNessie img (x, y) = do
      let chunk = map (slice x (x + cols - 1)) $ slice y (y + rows - 1) img
      all (uncurry isRowMatch) $ zip nessie chunk
    isRowMatch nr cr = do
      all (\(nc, cc) -> nc == 0 || (nc == 1 && cc == 1)) $ zip nr cr

nessie =
  tileToInts
    [ "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   "
    ]

part1 :: String -> String
part1 =
  show
    . product
    . map tileId
    . getCornerTiles
    . concatMap (permutations . parse)
    . splitOn "\n\n"

part2 :: String -> String
part2 =
  show
    . minimum
    . map (filterNessie . contents)
    . permutations
    . (\i -> Tile {tileId = 0, contents = i})
    . buildImage
    . head
    . filter isSquare
    . concatMap (uncurry buildMap)
    . initialize
    . map (permutations . parse)
    . splitOn "\n\n"
