module Day14 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, manhattan, traceCoordMap)
import Data.Either (fromRight)
import Data.List (findIndex, iterate')
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepBy)
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void T.Text

data Robot = Robot Coord Coord deriving (Show)

main :: IO ()
main =
  let bounds = (101, 103)
   in mkAoCMain 2024 14 (part1 bounds) (part2 bounds)

parseInt :: Parser Int
parseInt = signed space decimal

parseRobot :: Parser Robot
parseRobot = do
  _ <- string "p="
  px <- parseInt
  _ <- string ","
  py <- parseInt
  _ <- string " v="
  vx <- parseInt
  _ <- string ","
  vy <- parseInt
  return $ Robot (px, py) (vx, vy)

parseRobots :: Parser [Robot]
parseRobots = parseRobot `sepBy` string "\n"

parse' :: T.Text -> [Robot]
parse' = fromRight [] . parse parseRobots "day 14" . T.strip

elapse :: Coord -> Int -> Robot -> Robot
elapse (mx, my) n (Robot (px, py) (vx, vy)) =
  Robot ((px + n * vx) `mod` mx, (py + n * vy) `mod` my) (vx, vy)

robotPosition :: Robot -> Coord
robotPosition (Robot p _) = p

quandrants :: (Int, Int) -> [Coord] -> [[Coord]]
quandrants bounds =
  concatMap (splitOnEitherSideOf snd)
    . splitOnEitherSideOf fst
  where
    splitOnEitherSideOf t xs =
      [ filter ((< (t bounds `div` 2)) . t) xs,
        filter ((> (t bounds `div` 2)) . t) xs
      ]

entropy :: [Robot] -> Int
entropy [] = 0
entropy (r : rs) = sum (map (manhattan (robotPosition r) . robotPosition) rs) + entropy rs

part1 :: (Int, Int) -> T.Text -> Int
part1 bounds =
  product
    . map length
    . quandrants bounds
    . map (robotPosition . elapse bounds 100)
    . parse'

part2 :: (Int, Int) -> T.Text -> Int
part2 bounds =
  (+ offset)
    . ( \rss ->
          let i = fromJust $ findIndex (\rs -> entropy rs < maxEntropy) rss
           in traceRobots (rss !! i) i
      )
    . iterate' (map (elapse bounds 1))
    . map (elapse bounds offset)
    . parse'
  where
    -- 7500 is a guess
    offset = 7500
    -- 6000000 is a guess too!
    maxEntropy = 6000000

traceRobots :: [Robot] -> a -> a
traceRobots rs =
  let m = (M.fromListWith (+) . map ((,1 :: Int) . robotPosition)) rs
   in traceCoordMap m
