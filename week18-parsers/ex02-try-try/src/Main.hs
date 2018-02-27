module Main where

import Data.Ratio ((%))
import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative (Alternative(..))

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

tryTry :: Parser (Either Rational Integer)
tryTry = (Left <$> try parseFraction) <|> (Right <$> decimal)

main :: IO ()
main = do
  print $ parseString (tryTry <* eof) mempty "123"
  print $ parseString (tryTry <* eof) mempty "123/456"
  print $ parseString (tryTry <* eof) mempty "123/456/789"
  print $ parseString (tryTry <* eof) mempty "123_456"
