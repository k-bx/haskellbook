#!/usr/bin/env stack
-- stack --resolver=lts-10.1 script
import Data.Semigroup
import Test.QuickCheck

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: TrivAssoc)
