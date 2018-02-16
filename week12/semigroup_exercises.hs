#!/usr/bin/env stack
-- stack script --resolver lts-10.1
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Arbitrary a, Show a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = fmap (\(a, b) -> Two a b) arbitrary

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc @(Identity (Sum Int)))
  quickCheck (semigroupAssoc @(Two (Sum Int) (Sum Int)))
