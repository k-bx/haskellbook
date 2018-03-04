module Main where

import Control.Monad (replicateM)
import Safe
import Text.Trifecta

-- aka area code
type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange
              LineNumber
  deriving (Eq, Show)

readOrImpossible :: Read a => Parser String -> Parser a
readOrImpossible p = do
  s <- p
  case readMay s of
    Nothing -> fail "Impossible!?"
    Just a -> return a

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- optional (try (digit *> char '-'))
  _ <- optional (char '(')
  npa <- readOrImpossible (replicateM 3 digit)
  _ <- optional (many (oneOf ")- "))
  exch <- readOrImpossible (replicateM 3 digit)
  _ <- optional (oneOf "- ")
  linenum <- readOrImpossible (replicateM 4 digit)
  return (PhoneNumber npa exch linenum)

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
