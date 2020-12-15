module Day12 (main, part1, part2) where

type Instruction = (Char, Int)

type Coord = (Int, Int)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

applyInstruction :: Coord -> Char -> Instruction -> (Coord, Char)
-- cardinal directions
applyInstruction (x, y) dir ('N', magnitude) = ((x, y + magnitude), dir)
applyInstruction (x, y) dir ('S', magnitude) = ((x, y - magnitude), dir)
applyInstruction (x, y) dir ('E', magnitude) = ((x + magnitude, y), dir)
applyInstruction (x, y) dir ('W', magnitude) = ((x - magnitude, y), dir)
-- forward
applyInstruction (x, y) dir@'N' ('F', magnitude) = ((x, y + magnitude), dir)
applyInstruction (x, y) dir@'S' ('F', magnitude) = ((x, y - magnitude), dir)
applyInstruction (x, y) dir@'E' ('F', magnitude) = ((x + magnitude, y), dir)
applyInstruction (x, y) dir@'W' ('F', magnitude) = ((x - magnitude, y), dir)
-- rotate left
applyInstruction (x, y) 'N' ('L', 90) = ((x, y), 'W')
applyInstruction (x, y) 'N' ('L', 180) = ((x, y), 'S')
applyInstruction (x, y) 'N' ('L', 270) = ((x, y), 'E')
applyInstruction (x, y) 'S' ('L', 90) = ((x, y), 'E')
applyInstruction (x, y) 'S' ('L', 180) = ((x, y), 'N')
applyInstruction (x, y) 'S' ('L', 270) = ((x, y), 'W')
applyInstruction (x, y) 'E' ('L', 90) = ((x, y), 'N')
applyInstruction (x, y) 'E' ('L', 180) = ((x, y), 'W')
applyInstruction (x, y) 'E' ('L', 270) = ((x, y), 'S')
applyInstruction (x, y) 'W' ('L', 90) = ((x, y), 'S')
applyInstruction (x, y) 'W' ('L', 180) = ((x, y), 'E')
applyInstruction (x, y) 'W' ('L', 270) = ((x, y), 'N')
-- rotate right
applyInstruction (x, y) 'N' ('R', 90) = ((x, y), 'E')
applyInstruction (x, y) 'N' ('R', 180) = ((x, y), 'S')
applyInstruction (x, y) 'N' ('R', 270) = ((x, y), 'W')
applyInstruction (x, y) 'S' ('R', 90) = ((x, y), 'W')
applyInstruction (x, y) 'S' ('R', 180) = ((x, y), 'N')
applyInstruction (x, y) 'S' ('R', 270) = ((x, y), 'E')
applyInstruction (x, y) 'E' ('R', 90) = ((x, y), 'S')
applyInstruction (x, y) 'E' ('R', 180) = ((x, y), 'W')
applyInstruction (x, y) 'E' ('R', 270) = ((x, y), 'N')
applyInstruction (x, y) 'W' ('R', 90) = ((x, y), 'N')
applyInstruction (x, y) 'W' ('R', 180) = ((x, y), 'E')
applyInstruction (x, y) 'W' ('R', 270) = ((x, y), 'S')

run :: Coord -> Char -> [Instruction] -> Coord
run (x, y) _ [] = (x, y)
run (x, y) dir (inst : rest) = do
  let ((x', y'), dir') = applyInstruction (x, y) dir inst
  run (x', y') dir' rest

-- Part 2

applyInstruction' :: Coord -> Coord -> Char -> Instruction -> (Coord, Coord, Char)
-- cardinal directions
applyInstruction' (sx, sy) (wx, wy) dir ('N', magnitude) = ((sx, sy), (wx, wy + magnitude), dir)
applyInstruction' (sx, sy) (wx, wy) dir ('S', magnitude) = ((sx, sy), (wx, wy - magnitude), dir)
applyInstruction' (sx, sy) (wx, wy) dir ('E', magnitude) = ((sx, sy), (wx + magnitude, wy), dir)
applyInstruction' (sx, sy) (wx, wy) dir ('W', magnitude) = ((sx, sy), (wx - magnitude, wy), dir)
-- forward
applyInstruction' (sx, sy) (wx, wy) dir ('F', magnitude) = ((sx + (wx * magnitude), sy + (wy * magnitude)), (wx, wy), dir)
-- rotate left
applyInstruction' (sx, sy) (wx, wy) dir ('L', 90) = ((sx, sy), (- wy, wx), dir)
applyInstruction' (sx, sy) (wx, wy) dir ('L', 180) = ((sx, sy), (- wx, - wy), dir)
applyInstruction' (sx, sy) (wx, wy) dir ('L', 270) = ((sx, sy), (wy, - wx), dir)
-- rotate right
applyInstruction' (sx, sy) (wx, wy) dir ('R', 90) = ((sx, sy), (wy, - wx), dir)
applyInstruction' (sx, sy) (wx, wy) dir ('R', 180) = ((sx, sy), (- wx, - wy), dir)
applyInstruction' (sx, sy) (wx, wy) dir ('R', 270) = ((sx, sy), (- wy, wx), dir)

run' :: Coord -> Coord -> Char -> [Instruction] -> Coord
run' (sx, sy) _ _ [] = (sx, sy)
run' (x, y) (wx, wy) dir (inst : rest) = do
  let ((sx', sy'), (wx', wy'), dir') = applyInstruction' (x, y) (wx, wy) dir inst
  run' (sx', sy') (wx', wy') dir' rest

parse :: String -> Instruction
parse (c : i) = (c, read i)

manhattan :: Coord -> Int
manhattan (x, y) = abs x + abs y

part1 :: String -> String
part1 = show . manhattan . run (0, 0) 'E' . map parse . lines

part2 :: String -> String
part2 = show . manhattan . run' (0, 0) (10, 1) 'E' . map parse . lines
