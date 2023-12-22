module AOC.CoordMap
  ( Coord,
    CoordMap,
    dbgCoordMap,
    dbgCoordMap',
    diagonals,
    east,
    inDirections,
    lookupManyWithKey,
    lookupWithKey,
    manhattan,
    neighbours4,
    neighbours8,
    north,
    northeast,
    northwest,
    readCoordMap,
    south,
    southeast,
    southwest,
    traceCoordMap,
    traceCoordMap',
    west,
  )
where

import Data.List (foldl', union)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

type Coord = (Int, Int)

type CoordMap a = Map.Map Coord a

readCoordMap :: [[a]] -> CoordMap a
readCoordMap =
  foldl'
    ( \m (y, line) ->
        Map.union
          m
          ( foldl'
              (\m' (x, char) -> Map.insert (x, y) char m')
              Map.empty
              $ zip [0 ..] line
          )
    )
    Map.empty
    . zip [0 ..]

lookupWithKey :: Ord a => a -> Map.Map a b -> Maybe (a, b)
lookupWithKey c m = Map.lookup c m >>= (\v -> Just (c, v))

lookupManyWithKey :: Ord a => [a] -> Map.Map a b -> [(a, b)]
lookupManyWithKey cs m = mapMaybe (`lookupWithKey` m) cs

north :: Coord -> Map.Map Coord b -> Maybe (Coord, b)
north (x, y) m = let c = (x, y - 1) in c `lookupWithKey` m

south :: Coord -> Map.Map Coord b -> Maybe (Coord, b)
south (x, y) m = let c = (x, y + 1) in c `lookupWithKey` m

east :: Coord -> Map.Map Coord b -> Maybe (Coord, b)
east (x, y) m = let c = (x + 1, y) in c `lookupWithKey` m

west :: Coord -> Map.Map Coord b -> Maybe (Coord, b)
west (x, y) m = let c = (x - 1, y) in c `lookupWithKey` m

northwest :: Coord -> Map.Map Coord b -> Maybe (Coord, b)
northwest (x, y) m = let c = (x - 1, y - 1) in c `lookupWithKey` m

northeast :: Coord -> Map.Map Coord b -> Maybe (Coord, b)
northeast (x, y) m = let c = (x + 1, y - 1) in c `lookupWithKey` m

southwest :: Coord -> Map.Map Coord b -> Maybe (Coord, b)
southwest (x, y) m = let c = (x - 1, y + 1) in c `lookupWithKey` m

southeast :: Coord -> Map.Map Coord b -> Maybe (Coord, b)
southeast (x, y) m = let c = (x + 1, y + 1) in c `lookupWithKey` m

inDirections :: [Coord -> Map.Map Coord b -> Maybe (Coord, b)] -> Coord -> CoordMap b -> [(Coord, b)]
inDirections directions c m = mapMaybe (\f -> f c m) directions

neighbours4 :: Coord -> CoordMap a -> [(Coord, a)]
neighbours4 = inDirections [north, south, east, west]

diagonals :: Coord -> CoordMap a -> [(Coord, a)]
diagonals = inDirections [northwest, northeast, southwest, southeast]

neighbours8 :: (Eq a, Show a) => Coord -> CoordMap a -> [(Coord, a)]
neighbours8 coord m = neighbours4 coord m `union` diagonals coord m

manhattan :: Coord -> Coord -> Int
manhattan (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

traceCoordMap :: Show a => CoordMap a -> c -> c
traceCoordMap m = traceCoordMap' m (const (Nothing :: Maybe String))

traceCoordMap' :: (Show a, Show b) => CoordMap a -> (Coord -> Maybe b) -> c -> c
traceCoordMap' m transform =
  trace
    ( concat
        [ ( concat
              [ getValue x y ++ " "
                | x <- [minX .. maxX]
              ]
          )
            ++ "\n"
          | y <- [minY .. maxY]
        ]
    )
  where
    coords = Map.keys m
    xs = map fst coords
    ys = map snd coords
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
    getValue :: Int -> Int -> String
    getValue x y =
      let filterOutQuotes = filter (/= '\'')
       in case transform (x, y) of
            Just t -> filterOutQuotes $ show t
            Nothing -> maybe " " (filterOutQuotes . show) (m Map.!? (x, y))

dbgCoordMap :: Show a => CoordMap a -> CoordMap a
dbgCoordMap m = traceCoordMap m m

dbgCoordMap' :: (Show a, Show b) => CoordMap a -> (Coord -> Maybe b) -> CoordMap a
dbgCoordMap' m transform = traceCoordMap' m transform m
