module Day06 (main, part1, part2) where

-- Part 1
import Data.Array
import Data.List
import Text.Regex.PCRE ((=~))

type Light = Int

type Coord = (Int, Int)

type LightGrid = Array Coord Light

type Instruction = (Light -> Light, Coord, Coord)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

getCoords :: String -> (Coord, Coord)
getCoords line = do
  let (_, _, _, fromx : fromy : tox : toy : _) = line =~ "([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)" :: (String, String, String, [String])
  ((read fromx, read fromy), (read tox, read toy))

-- turnOn :: Light -> Light
-- turnOn = const 1

-- turnOff :: Light -> Light
-- turnOff = const 0

-- toggle :: Light -> Light
-- toggle = (1 -)

update :: Ix i => (e -> e) -> Array i e -> [i] -> Array i e
update f a indices = accum (flip ($)) a (zip indices (repeat f))

executeInstruction :: LightGrid -> Instruction -> LightGrid
executeInstruction grid (operation, bottomLeft, topRight) =
  update operation grid (range (bottomLeft, topRight))

executeInstructions :: [Instruction] -> LightGrid
executeInstructions = foldl' executeInstruction initialGrid
  where
    initialGrid = listArray ((0, 0), (999, 999)) $ repeat 0

parse :: (Light -> Light) -> (Light -> Light) -> (Light -> Light) -> String -> Instruction
parse turnOn turnOff toggle str
  | "turn off" `isPrefixOf` str = do
    let (from, to) = getCoords str
    (turnOff, from, to)
  | "turn on" `isPrefixOf` str = do
    let (from, to) = getCoords str
    (turnOn, from, to)
  | "toggle" `isPrefixOf` str = do
    let (from, to) = getCoords str
    (toggle, from, to)

part1 = length . filter (== 1) . elems . executeInstructions . map (parse (const 1) (const 0) (1 -)) . lines

-- part2 :: String -> Int
part2 = sum . elems . executeInstructions . map (parse (+ 1) (\l -> max 0 (l - 1)) (+ 2)) . lines
