{-# LANGUAGE ParallelListComp #-}

module Phone where

import Data.Char
import Data.Foldable
import Data.List

type Presses = Int

data Digit
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Zero
  | STAR
  | HASH
  deriving (Eq, Show)

dict :: [(Digit, String)]
dict =
  [ (One, "")
  , (Two, "abc")
  , (Three, "def")
  , (Four, "ghi")
  , (Five, "jkl")
  , (Six, "mno")
  , (Seven, "pqrs")
  , (Eight, "tuv")
  , (Nine, "wxyz")
  , (Zero, "+_")
  , (HASH, ".,")
  ]

type DaPhone = [(Char, [(Digit, Presses)])]

phone :: DaPhone
phone =
  lowerCharPresses dict ++
  (upperCharPresses . filter (\(x, _) -> x /= Zero && x /= HASH) $ dict)
  where
    lowerCharPresses =
      concatMap
        (\(k, vs) -> [(v, [(k, i)]) | v <- vs | i <- [1 .. (length vs)]])
    upperCharPresses =
      concatMap
        (\(k, vs) ->
           [ (v, [(STAR, 1), (k, i)])
           | v <- map toUpper vs
           | i <- [1 .. (length vs)]
           ])

--phone' =
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone char = head . map snd . filter (\x -> fst x == char) $ phone

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostPopularLetter :: String -> Char
mostPopularLetter s =
  fst .
  maximumBy (\(_, count) (_, count') -> compare count count') .
  nubBy (\(c, _) (c', _) -> c == c') .
  map (\c -> (c, length . filter (== c) $ s)) $
  s

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat
