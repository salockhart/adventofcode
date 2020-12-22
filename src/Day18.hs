module Day18 (main, part1, part2) where

-- Heavily borrowed from https://markkarpov.com/tutorial/megaparsec.html#parsing-expressions

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Either (rights)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, choice, parse)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Int Int
  | Sum Expr Expr
  | Product Expr Expr
  deriving (Eq, Ord, Show)

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[binary "*" Product, binary "+" Sum]]

operatorTable' :: [[Operator Parser Expr]]
operatorTable' = [[binary "+" Sum], [binary "*" Product]]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable
  where
    pTerm :: Parser Expr
    pTerm = choice [parens pExpr, pInteger]

pExpr' :: Parser Expr
pExpr' = makeExprParser pTerm operatorTable'
  where
    pTerm :: Parser Expr
    pTerm = choice [parens pExpr', pInteger]

calculate :: Expr -> Int
calculate expr = case expr of
  Int a -> a
  Sum lhs rhs -> calculate lhs + calculate rhs
  Product lhs rhs -> calculate lhs * calculate rhs

part1 :: String -> String
part1 = show . sum . solve . lines
  where
    solve = map calculate . rights . map (parse pExpr "day 18")

part2 :: String -> String
part2 = show . sum . solve . lines
  where
    solve = map calculate . rights . map (parse pExpr' "day 18")
