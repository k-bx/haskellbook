{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Safe
import Text.Groom
import Text.RawString.QQ (r)
import Text.Trifecta

data Activity = Activity
  { time :: TimeOfDay
  , name :: Text
  } deriving (Show, Eq)

data DayLog = DayLog
  { day :: Day
  , activities :: [Activity]
  } deriving (Show, Eq)

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
  time <-
    readOrImpossible (fmap (++ ":00") (some (noneOf " "))) :: Parser TimeOfDay
  _ <- spaces
  text <-
    (manyTill (noneOf "\n") ((void (string "--")) <|> (void (oneOf "\n"))))
  _ <- skipTillNextLine
  return (Activity time (T.strip (T.pack text)))

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
21:16 Read2
21:17 Read3
21:18 Read4
22:00 Sleep
|]

type MinutesSpent = Int

type PerActivityMap = Map Text MinutesSpent

todToMinutesSpent :: TimeOfDay -> MinutesSpent
todToMinutesSpent =
  fromIntegral .
  (`div` (1000000000000 * 60)) . diffTimeToPicoseconds . timeOfDayToTime

dayActivitiesToMinutesSpent :: [Activity] -> [(Text, MinutesSpent)]
dayActivitiesToMinutesSpent allActivities =
  let activitiesIndexed :: [(Int, Activity)]
      activitiesIndexed = zip [0 ..] allActivities
      getTimeSpent :: Int -> [(Int, Activity)] -> MinutesSpent
      getTimeSpent i acts =
        let actTime = time (snd (acts `at` i))
            mnextActTime = fmap time (lookup (i + 1) acts)
            actTimeSecs = todToMinutesSpent actTime
            nextActTimeSecs =
              fromMaybe (60 * 24) (fmap todToMinutesSpent mnextActTime)
            diff = nextActTimeSecs - actTimeSecs
        in if diff < 0
             then 0
             else diff
      toSpentPair ::
           [(Int, Activity)] -> (Int, Activity) -> (Text, MinutesSpent)
      toSpentPair acts (i, act) = (name act, getTimeSpent i acts)
  in map (toSpentPair activitiesIndexed) activitiesIndexed

main :: IO ()
main = do
  let res = parseString parseLog mempty example
  putStrLn $ groom $ res
  case res of
    Failure _ -> putStrLn ":("
    Success log -> do
      let perActivitySpent :: [(Text, MinutesSpent)]
          perActivitySpent =
            concatMap (dayActivitiesToMinutesSpent . activities) log
          perActivitySpentMap :: Map Text MinutesSpent
          perActivitySpentMap = M.fromListWith (+) perActivitySpent
      putStrLn $ groom $ perActivitySpentMap
      let avgActivitiesSpent :: [Activity] -> MinutesSpent
          avgActivitiesSpent xs =
            let spent :: [MinutesSpent]
                spent = map snd (dayActivitiesToMinutesSpent xs)
            in sum spent `div` length spent
      let avgPerActivity :: Map Day MinutesSpent
          avgPerActivity =
            M.fromListWith
              (+)
              (map (\x -> (day x, avgActivitiesSpent (activities x))) log)
      putStrLn $ groom $ avgPerActivity
      return ()
