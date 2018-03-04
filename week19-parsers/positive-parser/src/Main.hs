module Main where

import Control.Applicative
import Data.Foldable (foldl')
import Safe (readMay)
import Text.Parser.Combinators
import Text.Trifecta

{- 2 -}
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

{- 3 -}
base10Integer' :: Parser Integer
base10Integer' =
  try (fmap (* (-1)) (char '-' *> base10Integer)) <|> base10Integer

main :: IO ()
main
  {- 2 -}
 = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
  {- 3 -}
  print $ parseString base10Integer' mempty "-123abc"
