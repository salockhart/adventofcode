module Day16 (main, part1, part2) where

import Data.Either (rights)
import Data.List (foldl')
import Data.Maybe (isNothing)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parse, sepBy)
import Text.Megaparsec.Char (alphaNumChar, digitChar, string)

data Aunt = Aunt
  { auntId :: Int,
    children :: Maybe Int,
    cats :: Maybe Int,
    samoyeds :: Maybe Int,
    pomeranians :: Maybe Int,
    akitas :: Maybe Int,
    vizslas :: Maybe Int,
    goldfish :: Maybe Int,
    trees :: Maybe Int,
    cars :: Maybe Int,
    perfumes :: Maybe Int
  }
  deriving (Show)

type Parser = Parsec Void String

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

targetAunt =
  Aunt
    { auntId = 0,
      children = Just 3,
      cats = Just 7,
      samoyeds = Just 2,
      pomeranians = Just 3,
      akitas = Just 0,
      vizslas = Just 0,
      goldfish = Just 5,
      trees = Just 3,
      cars = Just 2,
      perfumes = Just 1
    }

parseProperty :: Parser (String, Int)
parseProperty = do
  name <- many alphaNumChar
  string ": "
  n <- many digitChar
  return (name, read n)

updateAunt :: Aunt -> (String, Int) -> Aunt
updateAunt a ("children", v) = a {children = Just v}
updateAunt a ("cats", v) = a {cats = Just v}
updateAunt a ("samoyeds", v) = a {samoyeds = Just v}
updateAunt a ("pomeranians", v) = a {pomeranians = Just v}
updateAunt a ("akitas", v) = a {akitas = Just v}
updateAunt a ("vizslas", v) = a {vizslas = Just v}
updateAunt a ("goldfish", v) = a {goldfish = Just v}
updateAunt a ("trees", v) = a {trees = Just v}
updateAunt a ("cars", v) = a {cars = Just v}
updateAunt a ("perfumes", v) = a {perfumes = Just v}

parseAunt :: Parser Aunt
parseAunt = do
  string "Sue "
  x <- many digitChar
  string ": "
  props <- parseProperty `sepBy` string ", "
  let baseAunt =
        Aunt
          { auntId = read x,
            children = Nothing,
            cats = Nothing,
            samoyeds = Nothing,
            pomeranians = Nothing,
            akitas = Nothing,
            vizslas = Nothing,
            goldfish = Nothing,
            trees = Nothing,
            cars = Nothing,
            perfumes = Nothing
          }
  return $ foldl' updateAunt baseAunt props

isAunt :: Aunt -> Aunt -> Bool
isAunt a b =
  children a `isEqualIfJust` children b
    && cats a `isEqualIfJust` cats b
    && samoyeds a `isEqualIfJust` samoyeds b
    && pomeranians a `isEqualIfJust` pomeranians b
    && akitas a `isEqualIfJust` akitas b
    && vizslas a `isEqualIfJust` vizslas b
    && goldfish a `isEqualIfJust` goldfish b
    && trees a `isEqualIfJust` trees b
    && cars a `isEqualIfJust` cars b
    && perfumes a `isEqualIfJust` perfumes b
  where
    isEqualIfJust a b = a == b || isNothing a || isNothing b

isAunt' :: Aunt -> Aunt -> Bool
isAunt' a b =
  children a `isEqualIfJust` children b
    && cats a `isGreaterThanIfJust` cats b
    && samoyeds a `isEqualIfJust` samoyeds b
    && pomeranians a `isLessThanIfJust` pomeranians b
    && akitas a `isEqualIfJust` akitas b
    && vizslas a `isEqualIfJust` vizslas b
    && goldfish a `isLessThanIfJust` goldfish b
    && trees a `isGreaterThanIfJust` trees b
    && cars a `isEqualIfJust` cars b
    && perfumes a `isEqualIfJust` perfumes b
  where
    isTrueIfJust :: (Maybe a1 -> Maybe a2 -> Bool) -> Maybe a1 -> Maybe a2 -> Bool
    isTrueIfJust f a b = f a b || isNothing a || isNothing b
    isEqualIfJust :: Maybe Int -> Maybe Int -> Bool
    isEqualIfJust = isTrueIfJust (==)
    isGreaterThanIfJust :: Maybe Int -> Maybe Int -> Bool
    isGreaterThanIfJust = isTrueIfJust (>)
    isLessThanIfJust :: Maybe Int -> Maybe Int -> Bool
    isLessThanIfJust = isTrueIfJust (<)

part1 :: String -> String
part1 =
  show
    . auntId
    . head
    . filter (`isAunt` targetAunt)
    . rights
    . map (parse parseAunt "day 16")
    . lines

part2 :: String -> String
part2 =
  show
    . auntId
    . head
    . filter (`isAunt'` targetAunt)
    . rights
    . map (parse parseAunt "day 16")
    . lines
