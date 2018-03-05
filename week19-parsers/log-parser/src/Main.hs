{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Groom
import Control.Monad (void)
import Safe (readMay)
import Control.Applicative
import Data.Text (Text)
import Data.Time
import Text.RawString.QQ (r)
import Text.Trifecta
import qualified Data.Text as T

data Activity =
  Activity TimeOfDay
           Text
  deriving (Show, Eq)

data DayLog =
  DayLog Day
         [Activity]
  deriving (Show, Eq)

type Log = [DayLog]

readOrImpossible :: Read a => Parser String -> Parser a
readOrImpossible p = do
  s <- p
  case readMay s of
    Nothing -> fail $ "Impossible!? Failed to parse a value: " ++ s
    Just a -> return a

skipTillNextLine :: Parser ()
skipTillNextLine = many (noneOf "\n") *> optional (char '\n') *> pure ()

parseActivity :: Parser Activity
parseActivity = do
  time <- readOrImpossible (fmap (++ ":00") (some (noneOf " "))) :: Parser TimeOfDay
  _ <- spaces
  text <- some (noneOf "\n")
  _ <- skipTillNextLine
  return (Activity time (T.pack text))

parseDayLog :: Parser DayLog
parseDayLog = do
  _ <- string "# "
  day <- readOrImpossible (some (noneOf " \n")) :: Parser Day
  _ <- skipTillNextLine
  activities <- many (optional comment *> try parseActivity)
  skipMany (comment <|> void space <|> void (char '\n'))
  return (DayLog day activities)

parseLog :: Parser Log
parseLog = do
  skipMany (comment <|> void space <|> void (char '\n'))
  many parseDayLog

comment :: Parser ()
comment = string "--" *> skipTillNextLine

example :: String
example =
  [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

main :: IO ()
main = do
  putStrLn $ groom $ parseString parseLog mempty example
