module Day13 (main, part1, part2) where

import AOC (mkAoCMain)
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Float (double2Int, int2Double)
import Text.Megaparsec (Parsec, parse, sepBy, some)
import Text.Megaparsec.Char (digitChar, string)

type Parser = Parsec Void T.Text

data Button = Button Double Double deriving (Show)

data Machine = Machine Button Button Double Double deriving (Show)

main :: IO ()
main = mkAoCMain 2024 13 part1 part2

parseButton :: String -> Parser Button
parseButton name = do
  _ <- string (T.pack ("Button " ++ name ++ ": X+"))
  x <- read <$> some digitChar
  _ <- string ", Y+"
  y <- read <$> some digitChar
  return $ Button x y

parseMachine :: Parser Machine
parseMachine = do
  buttonA <- parseButton "A"
  _ <- string "\n"
  buttonB <- parseButton "B"
  _ <- string "\n"
  _ <- string "Prize: X="
  prizeX <- read <$> some digitChar
  _ <- string ", Y="
  prizeY <- read <$> some digitChar
  return $ Machine buttonA buttonB prizeX prizeY

parseMachines :: Parser [Machine]
parseMachines = parseMachine `sepBy` "\n\n"

parse' :: T.Text -> [Machine]
parse' =
  fromRight []
    . parse parseMachines "day 13"

solve :: Machine -> (Double, Double)
solve (Machine (Button ax ay) (Button bx by) x y) = do
  let ta = ((y * bx) - (x * by)) / ((ay * bx) - (ax * by))
  let tb = (y - ta * ay) / by
  (ta, tb)

maybeInt :: Double -> Maybe Int
maybeInt f = if f == int2Double (double2Int f) then Just (double2Int f) else Nothing

maybeInts :: (Double, Double) -> Maybe (Int, Int)
maybeInts (a, b) = do
  a' <- maybeInt a
  b' <- maybeInt b
  return (a', b')

cost :: (Num a) => a -> a -> a
cost a b = 3 * a + b

part1 :: T.Text -> Int
part1 =
  sum
    . map (uncurry cost)
    . mapMaybe (maybeInts . solve)
    . parse'

part2 :: T.Text -> Int
part2 =
  sum
    . map (uncurry cost)
    . mapMaybe (maybeInts . solve . updatePrizes)
    . parse'
  where
    updatePrizes (Machine a b x y) = Machine a b (x + 10000000000000) (y + 10000000000000)
