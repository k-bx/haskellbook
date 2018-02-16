import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Checkers (EqProp(..), quickBatch, eq)
import Test.QuickCheck.Classes

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
                                                 where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
                                                               where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r
  -- traverse f (Node l a r) = liftA3 Node (traverse f l) (f a) (traverse f r)

arbitrarySized n = do
  goRecurse <- arbitrary :: Gen Bool
  i <- choose (0, n `div` 3)
  j <- choose (0, n `div` 3)
  k <- choose (0, n `div` 3)
  b2 <- arbitrarySized (n `div` 3)
  if not goRecurse
    then frequency [(1, pure Empty), (2, fmap Leaf arbitrary)]
    else do
      b1 <- arbitrarySized i
      -- b2 <- arbitrarySized j
      b3 <- arbitrarySized k
      return (Node b1 b2 b3)
  -- n <- choose (0, m)
  -- case n of
  --   0 -> pure Empty
  --   1 -> Leaf <$> arbitrary
  --   _ ->
  --     Node <$> arbitrarySized (n - 1) <*> arbitrarySized (n - 1) <*>
  --     arbitrarySized (n - 1)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbitrarySized

main :: IO ()
main = do
  let trigger2 :: Tree (Int, Int, [Int])
      trigger2 = undefined
  quickBatch (traversable trigger2)
-- main = undefined
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- module Main where
-- import Data.Monoid
-- import Control.Applicative (liftA3)
-- import Test.QuickCheck (Arbitrary(..))
-- import Test.QuickCheck.Checkers (EqProp(..), quickBatch, eq)
-- import Test.QuickCheck.Classes
-- newtype Identity a =
--   Identity a
--   deriving newtype (Eq, Ord, Show, Arbitrary, EqProp)
--   deriving Functor
--   deriving Foldable
-- -- instance Functor Identity where
-- --   fmap f (Identity x) = Identity (f x)
-- -- instance Foldable Identity where
-- --   foldr f b (Identity x) = f x b
-- instance Traversable Identity where
--   traverse f (Identity x) = fmap Identity (f x)
-- newtype Constant a b = Constant
--   { getConstant :: a
--   } deriving newtype (Show, Eq, Ord, Arbitrary, EqProp)
--   deriving Functor
--   deriving Foldable
-- instance Traversable (Constant a) where
--   traverse _ (Constant a) = pure (Constant a)
-- data Optional a
--   = Nada
--   | Yep a
--   deriving (Show, Eq, Ord)
--   deriving Functor
--   deriving Foldable
-- maybeToOptional Nothing = Nada
-- maybeToOptional (Just a) = (Yep a)
-- instance Arbitrary a => Arbitrary (Optional a) where
--   arbitrary = maybeToOptional <$> arbitrary
-- instance (Eq a, EqProp a) => EqProp (Optional a) where
--   (=-=) = eq
-- instance Traversable Optional where
--   traverse _ Nada = pure Nada
--   traverse f (Yep x) = Yep <$> f x
-- data List a = Nil
--             | Cons a (List a)
--   deriving (Show, Eq, Ord)
--   deriving Functor
--   deriving Foldable
-- instance Traversable List where
--   -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse _ Nil = pure Nil
--   traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
-- listToList' [] = Nil
-- listToList' (x:xs) = Cons x (listToList' xs)
-- instance Arbitrary a => Arbitrary (List a) where
--   arbitrary = listToList' <$> arbitrary
-- instance (Eq a, EqProp a) => EqProp (List a) where
--   (=-=) = eq
-- data Three a b c = Three a b c
--   deriving (Show, Eq, Ord)
--   deriving Functor
--   deriving Foldable
-- instance Traversable (Three a b) where
--   -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f (Three a b c) = Three a b <$> f c
-- instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
--   arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
--   -- arbitrary = liftA3 Three
-- instance (Eq a, EqProp a, Eq b, EqProp b, Eq c, EqProp c) => EqProp (Three a b c) where
--   (=-=) = eq
-- data Big a b = Big a b b
--   deriving (Show, Eq, Ord)
--   deriving Functor
--   deriving Foldable
-- instance Monoid a => Applicative (Big a) where
--   pure x = Big mempty x x
--   (Big x f g) <*> (Big y y2 y3) = Big (x <> y) (f y2) (g y3)
-- instance Traversable (Big a) where
--   -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f (Big a b c) = Big a <$> f b <*> f c
-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
--   arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary
--   -- arbitrary = liftA3 Three
-- instance (Eq a, EqProp a, Eq b, EqProp b) => EqProp (Big a b) where
--   (=-=) = eq
-- main :: IO ()
-- main = do
--   putStrLn "hello world"
--   -- let trigger2 :: Identity (Int, Int, [Int])
--   --     trigger2 = undefined
--   -- quickBatch (traversable trigger2)
--   -- let trigger3 :: Constant (Int, Int) (Int, Int, [Int])
--   --     trigger3 = undefined
--   -- quickBatch (traversable trigger3)
--   -- let trigger4 :: Optional (Int, Int, [Int])
--   --     trigger4 = undefined
--   -- quickBatch (traversable trigger4)
--   -- let trigger5 :: List (Int, Int, [Int])
--   --     trigger5 = undefined
--   -- quickBatch (traversable trigger5)
--   -- let trigger6 :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
--   --     trigger6 = undefined
--   -- quickBatch (traversable trigger6)
--   let trigger7 :: Big (Int, Int, [Int]) (Int, Int, [Int])
--       trigger7 = undefined
--   quickBatch (traversable trigger7)
