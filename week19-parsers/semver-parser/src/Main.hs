module Main where

import Text.Trifecta
import Control.Applicative

--import Text.Parser.Combinators
data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Show, Eq)

instance Ord NumberOrString where
  compare (NOSI _) (NOSS _) = GT
  compare (NOSS _) (NOSI _) = LT
  compare (NOSS s1) (NOSS s2) = compare s1 s2
  compare (NOSI n1) (NOSI n2) = compare n1 n2

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer =
  SemVer Major
         Minor
         Patch
         Release
         Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare (SemVer maj1 min1 pat1 rel1 met1) (SemVer maj2 min2 pat2 rel2 met2) =
    ifEqElse (compare maj1 maj2) $
    ifEqElse (compare min1 min2) $
    ifEqElse (compare pat1 pat2) $
    ifEqElse (compare rel1 rel2) $
    ifEqElse (compare met1 met2) $ EQ
    where
      ifEqElse c moveOn = if c == EQ then moveOn else c

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = (NOSI <$> try natural) <|> (NOSS <$> some alphaNum)

parseRelease :: Parser Release
parseRelease = parseNumberOrString `sepBy` char '.'

parseMeta :: Parser Metadata
parseMeta = parseNumberOrString `sepBy` char '.'

parseSemVer :: Parser SemVer
parseSemVer = do
  maj <- natural
  _ <- char '.'
  min <- natural
  _ <- char '.'
  patch <- natural
  mrel <- optional (char '-' *> parseRelease)
  case mrel of
    Nothing -> return (SemVer maj min patch [] [])
    Just rel -> do
      mmeta <- optional (char '+' *> parseMeta)
      case mmeta of
        Nothing -> return (SemVer maj min patch rel [])
        Just meta -> return (SemVer maj min patch rel meta)

main :: IO ()
main = do
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
