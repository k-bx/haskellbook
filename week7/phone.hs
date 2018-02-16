module Main where

-- | Few tricks I've done here:
-- - `on` is a nice function, often used
--   when you want to sort by some field, like
--
--   >>> sortBy (compare `on` snd) [("foo", 10), ("bar", 20)]
--
-- - if you want to sort backwards, you just `flip compare`, this way
--   `compare` now takes the arguments backwards (swaps them), thus
--   `(>) a b` becomes `(>) b a`, negating the result
import Data.Char (isUpper, toLower)
import Data.Function (on)
import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (catMaybes)

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data DaPhone =
  DaPhone [(Digit, [Char])]

daPhone :: DaPhone
daPhone =
  DaPhone
    [ ('1', "1")
    , ('2', "abc2")
    , ('3', "def3")
    , ('4', "ghi4")
    , ('5', "jkl5")
    , ('6', "mno6")
    , ('7', "pqrs7")
    , ('8', "tuv8")
    , ('9', "wxyz9")
    , ('*', "*^")
    , ('0', " 0+_")
    , ('#', "#.,")
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

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone pairs) c'
  | isUpper c' = ('*', 1) : low (toLower c') : []
  | otherwise = [low c']
  where
    low c =
      case catMaybes (map (toTaps c) pairs) of
        [] -> error ("Couldn't find a match for " ++ [c])
        (x:_) -> x
    toTaps c (digit, symbs) =
      case elemIndex c symbs of
        Nothing -> Nothing
        Just i -> Just (digit, i + 1)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone xs = concatMap (reverseTaps phone) xs

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . (map snd)

mostPopularLetter :: String -> Char
mostPopularLetter xs = fst (headDef err (sortByPopularity (counts xs)))
  where
    err = error "Not enough letters"

counts :: Ord a => [a] -> [(a, Int)]
counts = map addLen . group . sort
  where
    addLen xss@(x:xs) = (x, length xs)

sortByPopularity :: [(a, Int)] -> [(a, Int)]
sortByPopularity = sortBy ((flip compare) `on` snd)

coolestLtr :: [String] -> Char
coolestLtr ss = mostPopularLetter (concat ss)

coolestWord :: [String] -> String
coolestWord cnvo =
  let words = concatMap splitToWords cnvo
      sortedWords :: [(String, Int)]
      sortedWords = sortByPopularity (counts words)
  in fst (headDef err sortedWords)
  where
    err = error "Not enough words"

splitToWords :: String -> [String]
splitToWords str = reverse (go [] str)
  where
    go acc [] = acc
    go acc rest =
      case span (/= ' ') rest of
        (word, []) -> word : acc
        (word, (_:tail)) -> go (word : acc) tail

headDef :: a -> [a] -> a
headDef a [] = a
headDef _ (x:_) = x

main :: IO ()
main = return ()
