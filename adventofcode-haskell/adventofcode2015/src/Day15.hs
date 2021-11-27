module Day15 (main, part1, part2) where

import Data.List (foldl')
import Text.Regex.PCRE ((=~))

data Ingredient = Ingredient
  { name :: String,
    capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
  }
  deriving (Show)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> Ingredient
parse str = do
  let (_, _, _, name : cap : dur : fla : tex : cal : _) = str =~ "(\\w+): capacity ([-0-9]+), durability ([-0-9]+), flavor ([-0-9]+), texture ([-0-9]+), calories ([-0-9]+)" :: (String, String, String, [String])
  Ingredient
    { name = name,
      capacity = read cap,
      durability = read dur,
      flavor = read fla,
      texture = read tex,
      calories = read cal
    }

recipes :: [Ingredient] -> [[(Ingredient, Int)]]
recipes ings =
  map (zip ings) $
    (filter ((== 100) . sum) . mapM (const [1 .. 100])) ings

scores :: [(Ingredient, Int)] -> [Int]
scores =
  foldl'
    ( \[c, d, f, t, cl] i ->
        [ c + capacity i,
          d + durability i,
          f + flavor i,
          t + texture i,
          cl + calories i
        ]
    )
    [0, 0, 0, 0, 0]
    . map
      ( \(i, n) ->
          Ingredient
            { name = name i,
              capacity = n * capacity i,
              durability = n * durability i,
              flavor = n * flavor i,
              texture = n * texture i,
              calories = n * calories i
            }
      )

finalScore :: [Int] -> Int
finalScore = product . map (\i -> if i < 0 then 0 else i) . take 4

part1 :: String -> String
part1 =
  show
    . maximum
    . map (finalScore . scores)
    . recipes
    . map parse
    . lines

part2 :: String -> String
part2 =
  show
    . maximum
    . map finalScore
    . filter ((== 500) . (!! 4))
    . map scores
    . recipes
    . map parse
    . lines
