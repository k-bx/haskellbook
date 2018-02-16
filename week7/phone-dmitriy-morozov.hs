module Phone where

import Data.Char
import Data.Foldable (maximumBy)
import Data.List (group, map, sort)
import Data.Map (fromListWith, toList)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data DaPhone =
  Phone [(Digit, String)]
  deriving (Show)

phone :: DaPhone
phone =
  Phone
    [ ('1', "")
    , ('2', "abc")
    , ('3', "def")
    , ('4', "ghi")
    , ('5', "jkl")
    , ('6', "mno")
    , ('7', "pqrs")
    , ('8', "tuv")
    , ('9', "wxyz")
    , ('*', "^")
    , ('0', "+ _")
    , ('#', ".,")
    ]

type Code = (Digit, Presses)

type CodeBook = Map.Map Char (Digit, Int)

codeBook :: DaPhone -> CodeBook
codeBook (Phone xs) = Map.fromList $ concatMap codes xs
  where
    codes (d, cs) = zipWith (\i c -> (c, (d, succ i))) [0 .. (length cs)] cs

reverseTaps :: CodeBook -> Char -> [Code]
reverseTaps codeBook c
  | isUpper c = ('*', 1) : code
  | otherwise = code
  where
    code =
      case Map.lookup c codeBook of
        Just code' -> [code']
        Nothing -> []

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone' = concatMap (reverseTaps cb)
  where
    cb = codeBook phone'

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, n) acc -> acc + n) 0

mostPopular :: Ord a => [a] -> (Int, a)
mostPopular xs = maximum [(length x, head x) | x <- groups]
  where
    groups = (group . sort) xs

mostPopularLetter :: String -> (Int, Char)
mostPopularLetter = mostPopular . filter isLetter

coolestLtr :: [String] -> Char
coolestLtr = (snd . mostPopularLetter . concat)

coolestWord :: [String] -> String
coolestWord = snd . mostPopular . words . (map toLower) . unlines

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "DA"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]
