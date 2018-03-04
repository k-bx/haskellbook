module Main where

import Safe (readMay)
import Text.Trifecta
import Data.Foldable (foldl')
import Text.Parser.Combinators

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

parseDigitInt :: Parser Int
parseDigitInt = do
  c <- parseDigit
  let mi = readMay [c] :: Maybe Int
  case mi of
    Nothing -> fail "Impossible!?"
    Just i -> return i

base10Integer :: Parser Integer
base10Integer = foldl' f 0 <$> (some parseDigit)
  where
    f :: Integer -> Char -> Integer
    f acc c = acc * 10 + read [c]

main :: IO ()
main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
