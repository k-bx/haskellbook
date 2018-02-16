{-# OPTIONS_GHC -Wall -Werror #-}

module DaPhone where

import Data.Char (isUpper, toLower)
import Data.List (elemIndex, foldl', intersperse)
import Data.Maybe (catMaybes)

data DaPhone =
  DaPhone [String]
  deriving (Eq, Show)

type Digit = Char -- The name is misleading, because '*' and '#' aren't digits.

type Presses = Int

daPhone :: DaPhone
daPhone =
  DaPhone
    [ "1"
    , "2abc"
    , "3def"
    , "4ghi"
    , "5jkl"
    , "6mno"
    , "7pqrs"
    , "8tuv"
    , "9wxyz"
    , "*^"
    , "0+ "
    , "#.,"
    ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keys) c = concat . catMaybes $ map taps keys
  where
    taps :: String -> Maybe [(Digit, Presses)]
    taps "" = error "Empty String"
    taps key@(k:_) =
      case elemIndex (toLower c) key of
        Nothing -> Nothing
        Just 0 -> Just [(k, length key)]
        Just i ->
          Just $
          if isUpper c
            then [('*', 1), (k, i)]
            else [(k, i)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldl' (flip $ (+) . snd) 0

convo :: [String]
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

maxCost :: Ord a => (a -> Presses) -> [a] -> (a, Presses)
maxCost _ [] = error "Empty list"
maxCost cost (x:xs) =
  let z0 = (x, cost x)
      f z a =
        if snd z < cost a
          then (a, cost a)
          else z
  in foldl' f z0 xs

charCost :: Char -> Presses
charCost = fingerTaps . reverseTaps daPhone

strCost :: String -> Presses
strCost = sum . map charCost

-- The name is misleading: there can be several letters with maximum
-- cost, but `maxCost` will only return the first one.
mostPopularLetter :: String -> Char
mostPopularLetter = fst . maxCost charCost

-- Ditto.
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = fst . maxCost strCost . words . concat . intersperse " "
