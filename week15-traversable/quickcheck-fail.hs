#!/usr/bin/env stack
-- stack script --resolver=lts-10.4 --package checkers --package QuickCheck
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor.Identity
import Data.Monoid
import Data.Traversable (fmapDefault)
import Data.Typeable
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a =
  S (n a)
    a
  deriving (Show, Eq)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Foldable n => Foldable (S n) where
  foldMap f (S x y) = foldMap f x <> f y

instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

instance ( Show a
         , Show (n a)
         , Applicative n
         , Testable (n Property)
         , EqProp a
         , Eq (n a)
         , EqProp (n a)
         ) =>
         EqProp (S n a) where
  (S x y) =-= (S p q)
    -- trace "=-=" $
    -- trace "x:" $
    -- traceShow x $
    -- trace "y:" $
    -- traceShow y $
    -- trace "p:" $
    -- traceShow p $
    -- trace "q:" $
    -- traceShow q $
    -- trace "x == p:" $
    -- traceShow (x == p) $
   =
     -- (property $ (=-=) x p)
     (property $ (=-=) <$> x <*> p)
                        -- .&.
                        -- (y =-= q)

main = do
  let fmapP ::
           (Functor f, EqProp (f b), Traversable f)
        => (a -> b)
        -> f a
        -> Property
      fmapP f x = f `fmap` x =-= f `fmapDefault` x
  let trigger8 :: S [] (Int, Int, [Int])
      trigger8 = undefined
  -- quickBatch (traversable trigger8)
  quickBatch (traversable2 trigger8)

traversable2 ::
     forall f a b m.
     ( Traversable f
     , Monoid m
     , Show (f a)
     , Show (f b)
     , Arbitrary (f a)
     , Arbitrary b
     , Arbitrary a
     , Arbitrary m
     , CoArbitrary a
     , EqProp (f b)
     , EqProp m
     )
  => f (a, b, m)
  -> TestBatch
traversable2 = const ("traversable", [("fmap", property fmapP)])
  where
    fmapP :: (a -> b) -> f a -> Property
    fmapP f x = f `fmap` x =-= f `fmapDefault` x
