module Day12 (main, part1, part2) where

import qualified Data.Bifunctor
import Data.Either (fromRight)
import Data.Void (Void)
import Text.JSON
  ( JSObject,
    JSValue (JSArray, JSNull, JSObject, JSString),
    Result (Ok),
    decode,
    encode,
    fromJSObject,
    fromJSString,
    toJSObject,
  )
import Text.Megaparsec (Parsec, choice, many, parse, some)
import Text.Megaparsec.Char (char, digitChar, printChar)

type Parser = Parsec Void String

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parseNonDigits :: Parser [a]
parseNonDigits = do
  printChar
  return []

parseDigits :: Parser [Int]
parseDigits = do
  x <- some $ choice [char '-', digitChar]
  return [read x]

getDigits :: Parser [[Int]]
getDigits = do
  many $ choice [parseDigits, parseNonDigits]

fromResult :: Result a -> a
fromResult (Ok x) = x

filterReds :: JSValue -> JSValue
filterReds (JSArray jsons) = JSArray (map filterReds jsons)
filterReds (JSObject x) =
  if isRedObj x
    then JSNull
    else
      JSObject $
        toJSObject (map (Data.Bifunctor.second filterReds) $ fromJSObject x)
  where
    isRedStr :: JSValue -> Bool
    isRedStr (JSString s) = fromJSString s == "red"
    isRedStr _ = False
    isRedObj :: JSObject JSValue -> Bool
    isRedObj json = any (isRedStr . snd) $ fromJSObject json
filterReds x = x

part1 :: String -> String
part1 = show . sum . concat . fromRight [] . parse getDigits "day 12"

part2 :: String -> String
part2 = part1 . encode . filterReds . fromResult . decode
