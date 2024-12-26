module Day15 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, CoordMapDirection, east, north, readCoordMap, south, west)
import AOC.Data.Tuple (fromList2)
import Data.Bifunctor (bimap)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 15 part1 part2

readMovement :: Char -> CoordMapDirection b
readMovement '^' = north
readMovement 'v' = south
readMovement '>' = east
readMovement '<' = west
readMovement _ = error "readMovement: invalid direction"

parse :: (Char -> String) -> T.Text -> ((Coord, CoordMap Char), [CoordMapDirection Char])
parse transformer =
  bimap
    ( ( \m ->
          let (start, _) = fromJust $ find ((== '@') . snd) $ M.toList m
           in (start, m)
      )
        . readCoordMap
        . lines
        . concatMap transformer
        . T.unpack
        . T.strip
    )
    (map readMovement . concat . lines . T.unpack)
    . fromList2
    . T.splitOn "\n\n"

solve :: (Coord, CoordMap Char) -> [CoordMapDirection Char] -> (Coord, CoordMap Char)
solve = foldl (\(c, m) d -> fromMaybe (c, m) (solve' (c, m) d))

solve' :: (Coord, M.Map Coord Char) -> CoordMapDirection Char -> Maybe (Coord, CoordMap Char)
solve' (c, m) d = case d c m of
  -- for empty spaces, we can move there
  Just (c', '.') -> return $ move c c' m
  -- for boxes, we can only move if that box can move
  Just (c', 'O') -> do
    (_, m') <- solve' (c', m) d
    return $ move c c' m'
  -- for the double wide boxes, try to move their other half first!
  Just (c', ']') -> do
    (otherHalf, _) <- west c' m
    (_, m') <- solve' (otherHalf, m) d
    (_, m'') <- solve' (c', m') d
    return $ move c c' m''
  Just (c', '[') -> do
    (otherHalf, _) <- east c' m
    (_, m') <- solve' (otherHalf, m) d
    (_, m'') <- solve' (c', m') d
    return $ move c c' m''
  _ -> Nothing

move :: Coord -> Coord -> CoordMap Char -> (Coord, CoordMap Char)
move from to m =
  let v = m M.! from
   in (to, M.union (M.fromList [(from, '.'), (to, v)]) m)

gpsSum :: (Char -> Bool) -> CoordMap Char -> Int
gpsSum p =
  sum
    . map (\(x, y) -> x + 100 * y)
    . M.keys
    . M.filter p

part1 :: T.Text -> Int
part1 =
  gpsSum (== 'O')
    . snd
    . uncurry solve
    . parse (: [])

part2 :: T.Text -> Int
part2 =
  gpsSum (== '[') -- sum from the top left corner of the double wide boxes
    . snd
    . uncurry solve
    . parse widen
  where
    widen '#' = "##"
    widen 'O' = "[]"
    widen '.' = ".."
    widen '@' = "@."
    widen '\n' = "\n" -- don't double up the newlines!
    widen _ = error "widen: invalid char"
