{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a =
  S (n a)
    a
  deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Foldable n => Foldable (S n) where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (S x y) = foldMap f x <> f y

instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

instance (Applicative n, Testable (n Property), EqProp a) =>
         EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p)
                        -- .&.
                        -- (y =-= q)

-- instance (Applicative n, EqProp (n a), Eq (n a), Eq a, EqProp a) =>
--          EqProp (S n a) where
--   (=-=) = eq

-- let s = S [-8,10,-7,1,8,-7,-9,-1] (-4)
-- let f = (Sum . (+3))
-- does this hold: `fmap f s == fmapDefault f s`?
--
-- fmapDefault f ≡ runIdentity . traverse (Identity . f)
--
-- fmap (Sum . (+3)) (S [-8,10,-7,1,8,-7,-9,-1] (-4))
-- == (Functor (S n))
-- foldMap (Sum . (+3)) [-8,10,-7,1,8,-7,-9,-1] <> f (-4)
-- == (Foldable [])
-- Sum {getSum = 11}
--
-- fmapDefault (Sum . (+3)) (S [-8,10,-7,1,8,-7,-9,-1] (-4))
-- ==
-- runIdentity (traverse (Identity . (Sum . (+3))) (S [-8,10,-7,1,8,-7,-9,-1] (-4)))
-- == Traversable (S n)
-- runIdentity (S <$> traverse (Identity . (Sum . (+3))) [-8,10,-7,1,8,-7,-9,-1] <*> (Identity . (Sum . (+3))) (-4))
-- == (eval `Identity . (Sum . (+3))) (-4)`)
-- runIdentity (S <$> traverse (Identity . (Sum . (+3))) [-8,10,-7,1,8,-7,-9,-1] <*> Identity (Sum {getSum = -1}))
-- == (eval `traverse (Identity . (Sum . (+3))) [-8,10,-7,1,8,-7,-9,-1]`)
-- runIdentity (S <$> Identity [Sum {getSum = -5},Sum {getSum = 13},Sum {getSum = -4},Sum {getSum = 4},Sum {getSum = 11},Sum {getSum = -4},Sum {getSum = -6},Sum {getSum = 2}] <*> Identity (Sum {getSum = -1}))
-- 

main = do
  putStrLn "hello"
  -- sample' (arbitrary :: Gen (S [] Int))
  let trigger8 :: S [] (Int, Int, [Int])
      trigger8 = undefined
  quickBatch (traversable trigger8)
