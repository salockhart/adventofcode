{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Day19 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.Tuple (fromList2)
import Data.Bifunctor (bimap)
import Data.Either (rights)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, some, try, (<|>))
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Part = (Int, Int, Int, Int)

type PartRange = ((Int, Int), (Int, Int), (Int, Int), (Int, Int))

partSum :: Part -> Int
partSum (x, m, a, s) = x + m + a + s

data Workflow
  = WorkflowBranch (Char, Char, Int) Workflow Workflow
  | WorkflowName String
  | PartAccepted
  | PartRejected

parse' :: T.Text -> (M.Map String Workflow, [Part])
parse' =
  bimap
    (M.fromList . rights . map (parse parseWorkflow "day 19") . T.lines)
    (rights . map (parse parsePart "day 19") . T.lines)
    . fromList2
    . T.splitOn "\n\n"

type Parser = Parsec Void T.Text

-- ex{x>10:one,m<20:two,a>30:R,A}
parseWorkflow :: Parser (String, Workflow)
parseWorkflow = do
  name <- some letterChar
  _ <- char '{'
  workflow <- parseWorkflow'
  _ <- char '}'
  return (name, workflow)
  where
    parseWorkflow' = try parseBranch <|> parseName
    parseBranch = do
      target <- letterChar
      operation <- char '<' <|> char '>'
      value <- decimal
      _ <- char ':'
      left <- parseName
      _ <- char ','
      right <- parseWorkflow'
      return $ WorkflowBranch (target, operation, value) left right
    parseName = do
      name <- some letterChar
      return $ case name of
        "A" -> PartAccepted
        "R" -> PartRejected
        x -> WorkflowName x

-- {x=787,m=2655,a=1222,s=2876}
parsePart :: Parser Part
parsePart = do
  _ <- string "{x="
  x <- decimal
  _ <- string ",m="
  m <- decimal
  _ <- string ",a="
  a <- decimal
  _ <- string ",s="
  s <- decimal
  _ <- string "}"
  return (x, m, a, s)

main :: IO ()
main = mkAoCMain 2023 19 part1 part2

executeWorkflow :: String -> M.Map String Workflow -> Part -> Bool
executeWorkflow name defined part@(x, m, a, s) = applyWorkflow (defined M.! name)
  where
    applyWorkflow workflow = case workflow of
      PartAccepted -> True
      PartRejected -> False
      WorkflowName next -> executeWorkflow next defined part
      WorkflowBranch predicate left right ->
        if executePredicate predicate
          then applyWorkflow left
          else applyWorkflow right
    executePredicate ('x', '<', value) = x < value
    executePredicate ('x', '>', value) = x > value
    executePredicate ('m', '<', value) = m < value
    executePredicate ('m', '>', value) = m > value
    executePredicate ('a', '<', value) = a < value
    executePredicate ('a', '>', value) = a > value
    executePredicate ('s', '<', value) = s < value
    executePredicate ('s', '>', value) = s > value
    executePredicate _ = error "couldn't build predicate"

executeWorkflow' :: String -> M.Map String Workflow -> PartRange -> Int
executeWorkflow'
  name
  defined = applyWorkflow (defined M.! name)
    where
      applyWorkflow
        workflow
        partRange@(x, m, a, s) = case workflow of
          PartAccepted -> (snd x - fst x + 1) * (snd m - fst m + 1) * (snd a - fst a + 1) * (snd s - fst s + 1)
          PartRejected -> 0
          WorkflowName next -> executeWorkflow' next defined partRange
          WorkflowBranch (target, op, value) left right -> do
            let (leftRange, rightRange) = splitPartRange
            executeBranch (left, leftRange) (right, rightRange)
            where
              splitPartRange = case op of
                '<' -> splitPartRange' value
                '>' -> swap $ splitPartRange' (value + 1)
                _ -> error "couldn't operation"
              splitPartRange' v = case target of
                'x' -> bimap' (,m,a,s) (splitRange x v)
                'm' -> bimap' (x,,a,s) (splitRange m v)
                'a' -> bimap' (x,m,,s) (splitRange a v)
                's' -> bimap' (x,m,a,) (splitRange s v)
                _ -> error "couldn't ranges"
      bimap' fn = bimap fn fn
      splitRange (minN, maxN) value = ((minN, value - 1), (value, maxN))
      executeBranch (left, lRange) (right, rRange) =
        applyWorkflow left lRange + applyWorkflow right rRange

part1 :: T.Text -> Int
part1 =
  sum
    . map partSum
    . ( \(defined, parts) ->
          filter (executeWorkflow "in" defined) parts
      )
    . parse'

part2 :: T.Text -> Int
part2 =
  ( \defined ->
      executeWorkflow'
        "in"
        defined
        ((1, 4000), (1, 4000), (1, 4000), (1, 4000))
  )
    . fst
    . parse'
