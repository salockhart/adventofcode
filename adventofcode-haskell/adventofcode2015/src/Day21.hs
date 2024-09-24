{-# LANGUAGE TupleSections #-}

module Day21 (main, part1, part2) where

import AOC.Data.List (combinations)
import AOC.Data.String (splitOn)
import Data.Tuple.Utils (thd3)

data Mob = Mob
  { hp :: Int,
    mobDamage :: Int,
    mobArmor :: Int
  }
  deriving (Show)

data Item = Item
  { name :: String,
    cost :: Int,
    itemDamage :: Int,
    itemArmor :: Int
  }
  deriving (Show)

weapons :: [Char]
weapons =
  "Weapons:    Cost  Damage  Armor\n\
  \Dagger        8     4       0\n\
  \Shortsword   10     5       0\n\
  \Warhammer    25     6       0\n\
  \Longsword    40     7       0\n\
  \Greataxe     74     8       0"

armors :: [Char]
armors =
  "Armor:      Cost  Damage  Armor\n\
  \Leather      13     0       1\n\
  \Chainmail    31     0       2\n\
  \Splintmail   53     0       3\n\
  \Bandedmail   75     0       4\n\
  \Platemail   102     0       5"

rings :: [Char]
rings =
  "Rings:      Cost  Damage  Armor\n\
  \Damage +1    25     1       0\n\
  \Damage +2    50     2       0\n\
  \Damage +3   100     3       0\n\
  \Defense +1   20     0       1\n\
  \Defense +2   40     0       2\n\
  \Defense +3   80     0       3"

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parseMob :: String -> Mob
parseMob str = do
  let [hp, mobDamage, mobArmor] = lines str
  Mob
    { hp = read . drop 12 $ hp,
      mobDamage = read . drop 8 $ mobDamage,
      mobArmor = read . drop 7 $ mobArmor
    }

weaponChoices :: ([Item], [Item], [Item])
weaponChoices = (parseEntries weapons, parseEntries armors, parseEntries rings)
  where
    parseEntries = map (parseItem . filter (/= "") . splitOn "  ") . drop 1 . lines
    parseItem [name, cost, itemDamage, itemArmor] =
      Item
        { name = name,
          cost = read cost,
          itemDamage = read itemDamage,
          itemArmor = read itemArmor
        }

possibleFights :: (Mob, ([Item], [Item], [Item])) -> [(Mob, Mob, Int)]
possibleFights (enemy, (weapons, armors, rings)) = do
  let possibleWeapons = filter ((== 1) . length) . combinations $ weapons
  let possibleArmors = filter ((< 2) . length) . combinations $ armors
  let possibleRings = filter ((< 3) . length) . combinations $ rings
  [(enemy, buildUser weapon armor ring, costOf weapon armor ring) | weapon <- possibleWeapons, armor <- possibleArmors, ring <- possibleRings]
  where
    buildUser :: [Item] -> [Item] -> [Item] -> Mob
    buildUser weapons armors rings =
      Mob
        { hp = 100,
          mobDamage = sumOfValue itemDamage [weapons, armors, rings],
          mobArmor = sumOfValue itemArmor [weapons, armors, rings]
        }
    costOf :: [Item] -> [Item] -> [Item] -> Int
    costOf weapons armors rings = sumOfValue cost [weapons, armors, rings]
    sumOfValue :: (Num c, Foldable t) => (a -> c) -> t [a] -> c
    sumOfValue f = sum . map f . concat

fight :: (Mob, Mob, Int) -> Bool
fight (enemy, me, c)
  | hp enemy <= 0 = True
  | hp me <= 0 = False
  | otherwise =
      fight
        ( enemy {hp = hp enemy - damageDealt me enemy},
          me {hp = hp me - damageDealt enemy me},
          c
        )
  where
    damageDealt :: Mob -> Mob -> Int
    damageDealt by to = max 1 (mobDamage by - mobArmor to)

part1 :: String -> String
part1 =
  show
    . minimum
    . map thd3
    . filter fight
    . possibleFights
    . (,weaponChoices)
    . parseMob

part2 :: String -> String
part2 =
  show
    . maximum
    . map thd3
    . filter (not . fight)
    . possibleFights
    . (,weaponChoices)
    . parseMob
