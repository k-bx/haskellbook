module Main where

import Text.Trifecta
import Text.Parser.Combinators (eof)
import Control.Applicative (Alternative(..))

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1'

-- read a single character '1', then die
one' :: Parser Char
one' = one >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters, -- '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

{- 1 -}

oneEof :: Parser Char
oneEof = one <* eof

oneTwoEof = do
  res <- oneTwo
  _ <- eof
  return res

{- 2 -}

oneTwoThree :: Parser String
oneTwoThree = (string "123" <|> string "12" <|> string "1") <* eof
-- oneTwoThree = do
--   res <- string "123" <|> string "12" <|> string "1"
--   eof
--   return res

{- 3 -}

myString :: String -> Parser String
myString [] = pure ""
myString (x:xs) = (:) <$> char x <*> myString xs
-- myString (x:xs) = do
--   r <- char x
--   rest <- myString xs
--   return (r:rest)

integerAndEof :: Result Integer
integerAndEof = parseString (integer <* eof) mempty "123"

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
