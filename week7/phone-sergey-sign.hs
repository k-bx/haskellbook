module Phone where

import Data.Char (isUpper, toLower)
import Data.Function (on)
import Data.List (groupBy, maximumBy, sort, sortBy)

convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

data KeyRule =
  KeyRule Digit
          [Digit]
  deriving (Show)

keyRules =
  [ KeyRule '1' []
  , KeyRule '2' "abc"
  , KeyRule '3' "def"
  , KeyRule '4' "ghi"
  , KeyRule '5' "jkl"
  , KeyRule '6' "mno"
  , KeyRule '7' "pqrs"
  , KeyRule '8' "tuv"
  , KeyRule '9' "wxyz"
  , KeyRule '*' "^"
  , KeyRule '0' "+ "
  , KeyRule '#' ".,"
  ]

makeRawDaPhone :: [KeyRule] -> [(Char, (Digit, Presses))]
makeRawDaPhone info = concatMap parceRule fullRules
  where
    fullRules = map keyRule2pair info
      where
        keyRule2pair (KeyRule d cs) = (d, cs ++ [d])
    parceRule (phoneKey, chars) = map wrapPair pairs
      where
        pairs = getCharAndPosition [] 1 chars
        wrapPair (letter, presses) = (letter, (phoneKey, presses))
    getCharAndPosition ::
         [(Digit, Presses)] -> Presses -> [Digit] -> [(Digit, Presses)]
    getCharAndPosition acc position (c:[]) = (c, position) : acc
    getCharAndPosition acc position (c:cs) =
      getCharAndPosition ((c, position) : acc) (position + 1) cs

data DaPhone =
  DaPhone [(Char, (Digit, Presses))]
  deriving (Show)

daPhone :: DaPhone
daPhone = DaPhone $ makeRawDaPhone keyRules

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone _) '^' = []
reverseTaps (DaPhone dict) c = maybeAsResult $ lookup (toLower c) dict
  where
    maybeAsResult Nothing = []
    maybeAsResult (Just r) = tryFixUpperCase [r]
    tryFixUpperCase =
      if isUpper c
        then (upperFlag :)
        else id
    upperFlag = ('*', 1)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostPopular :: String -> (Char, Presses)
mostPopular = mostPopularTap . cellPhonesDead daPhone

mostPopularTap :: Ord a => [(a, b)] -> (a, b)
mostPopularTap = maximumBy (compare `on` fst)

mostPopularLetter :: String -> Char -- Maybe Char?
mostPopularLetter = fst . mostPopular

mostPopularKey :: [String] -> Char
mostPopularKey ss = fst $ mostPopularTap taps
  where
    taps = map mostPopular ss

coolestLtr :: [String] -> Char
coolestLtr ss = mostUsed letters
  where
    letters = concat $ concatMap words ss

coolestWord :: [String] -> String
coolestWord ss = mostUsed lowerCaseText
  where
    text = concatMap words ss
    lowerCaseText = map (map toLower) text

mostUsed :: Ord a => [a] -> a
mostUsed ss =
  head $ head $ sortBy (flip compare `on` length) $ groupBy (==) $ sort ss
