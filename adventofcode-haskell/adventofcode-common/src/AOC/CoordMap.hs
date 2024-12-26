module AOC.CoordMap where

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

lookupWithKey :: (Ord a) => a -> Map.Map a b -> Maybe (a, b)
lookupWithKey c m = Map.lookup c m >>= (\v -> Just (c, v))

lookupManyWithKey :: (Ord a) => [a] -> Map.Map a b -> [(a, b)]
lookupManyWithKey cs m = mapMaybe (`lookupWithKey` m) cs

type CoordMapDirection b = Coord -> CoordMap b -> Maybe (Coord, b)

north :: CoordMapDirection b
north (x, y) m = let c = (x, y - 1) in c `lookupWithKey` m

south :: CoordMapDirection b
south (x, y) m = let c = (x, y + 1) in c `lookupWithKey` m

east :: CoordMapDirection b
east (x, y) m = let c = (x + 1, y) in c `lookupWithKey` m

west :: CoordMapDirection b
west (x, y) m = let c = (x - 1, y) in c `lookupWithKey` m

northwest :: CoordMapDirection b
northwest (x, y) m = let c = (x - 1, y - 1) in c `lookupWithKey` m

northeast :: CoordMapDirection b
northeast (x, y) m = let c = (x + 1, y - 1) in c `lookupWithKey` m

southwest :: CoordMapDirection b
southwest (x, y) m = let c = (x - 1, y + 1) in c `lookupWithKey` m

southeast :: CoordMapDirection b
southeast (x, y) m = let c = (x + 1, y + 1) in c `lookupWithKey` m

inDirections :: [CoordMapDirection b] -> Coord -> CoordMap b -> [(Coord, b)]
inDirections directions c m = mapMaybe (\f -> f c m) directions

neighbours4 :: Coord -> CoordMap a -> [(Coord, a)]
neighbours4 = inDirections [north, south, east, west]

diagonals :: Coord -> CoordMap a -> [(Coord, a)]
diagonals = inDirections [northwest, northeast, southwest, southeast]

neighbours8 :: (Eq a, Show a) => Coord -> CoordMap a -> [(Coord, a)]
neighbours8 coord m = neighbours4 coord m `union` diagonals coord m

manhattan :: Coord -> Coord -> Int
manhattan (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

bounds :: CoordMap a -> (Int, Int, Int, Int)
bounds cm =
  let keys = Map.keys cm
      xs = map fst keys
      ys = map snd keys
   in ( minimum xs,
        maximum xs,
        minimum ys,
        maximum ys
      )

-- |
-- Returns the area of the polygon described by the provided
-- list of points.
--
-- See also: https://en.wikipedia.org/wiki/Shoelace_formula
shoelaceTheorem :: [Coord] -> Int
shoelaceTheorem = abs . (`div` 2) . sum . shoelace' . bookend
  where
    bookend xs = last xs : xs
    shoelace' ((x0, y0) : p1@(x1, y1) : rest) =
      ((y0 + y1) * (x0 - x1)) : shoelace' (p1 : rest)
    shoelace' _ = []

-- |
-- Returns the number of points internal to the polygon described
-- by the provided list of points.
--
-- Note: this requires that the list of points describes *all* points
-- on the boundary of the polygon. If it only provides vertices and not
-- any points that lie on the edges between them, this will be incorrect.
--
-- See also: https://en.wikipedia.org/wiki/Pick%27s_theorem
picksTheorem :: [Coord] -> Int
picksTheorem path = picksTheorem' (shoelaceTheorem path) (length path)

-- |
-- A variant of 'picksTheorem' that allows for passing the values for area
-- and the number of boundary points directly.
picksTheorem' :: Int -> Int -> Int
picksTheorem' area numBoundary = area + 1 - (numBoundary `div` 2)

traceCoordMap :: (Show a) => CoordMap a -> c -> c
traceCoordMap m = traceCoordMap' m (const (Nothing :: Maybe String))

traceCoordMap' :: (Show a, Show b) => CoordMap a -> (Coord -> Maybe b) -> c -> c
traceCoordMap' m transform =
  trace
    ( concat
        ( "\n\n"
            : [ ( concat
                    [ getValue x y ++ " "
                      | x <- [minX .. maxX]
                    ]
                )
                  ++ "\n"
                | y <- [minY .. maxY]
              ]
        )
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

dbgCoordMap :: (Show a) => CoordMap a -> CoordMap a
dbgCoordMap m = traceCoordMap m m

dbgCoordMap' :: (Show a, Show b) => CoordMap a -> (Coord -> Maybe b) -> CoordMap a
dbgCoordMap' m transform = traceCoordMap' m transform m
