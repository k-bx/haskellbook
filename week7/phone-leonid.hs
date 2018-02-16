module Ch11Phone where

import Data.Char
import Data.List
import Data.Ord

type Digit = Char

type Letters = [Char]

type Presses = Int

data DaPhone =
  DaPhone [(Digit, Letters)]
  deriving (Eq, Show)

phone =
  DaPhone
    [ ('1', "")
    , ('2', "abc")
    , ('3', "def")
    , ('4', "ghi")
    , ('5', "jkl")
    , ('6', "mno")
    , ('7', "pqrs")
    , ('8', "tuv")
    , ('9', "wxyz")
    , ('0', "+ ")
    , ('*', "^")
    , ('#', ".,")
    ]

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

getTaps :: Eq a => a -> [a] -> Int
getTaps x xs =
  case elemIndex x xs of
    Just v -> v + 1
    Nothing -> 0

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone l) c =
  case find (elem (toLower c) . snd) l of
    Just lets ->
      if isUpper c
        then ('*', 1) : taps
        else taps
      where taps = [(fst lets, getTaps (toLower c) (snd lets))]
    Nothing -> []

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps l = sum (map snd l)

mostPopularLetter :: String -> Char
mostPopularLetter str = fst (mostPopularAAndCount str)

coolestLtr :: [String] -> Char
coolestLtr str = fst (maxBy snd (map mostPopularAAndCount str))

coolestWord :: [String] -> String
coolestWord strs = fst (mostPopularAAndCount (concatMap words strs))

--util functions
aCount :: Eq a => [a] -> [(a, Int)]
aCount str = result
  where
    indexed = map (\x -> (x, 1)) str
    grouped = groupBy (\x y -> fst x == fst y) indexed
    result = fmap (\x -> (fst (head x), length x)) grouped

maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing

mostPopularAAndCount :: Eq a => [a] -> (a, Int)
mostPopularAAndCount str = maxBy snd (aCount str)
