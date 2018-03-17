{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative

newtype Compose f g a = Compose
  { runCompose :: f (g a)
  }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose comp) = Compose ((fmap . fmap) f comp)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose ((liftA2 (<*>) :: (f (g (a -> b)) -> f (g a) -> f (g b))) f a)
  liftA2 ::
       forall a b c.
       (a -> b -> c)
    -> Compose f g a
    -> Compose f g b
    -> Compose f g c
  liftA2 f (Compose x) (Compose y) =
    Compose (liftA2 (liftA2 f :: g a -> g b -> g c) x y :: f (g c))
  -- (Compose f) <*> (Compose a) = Compose ((<*>) <$> f <*> a)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap ::
       forall a m. (Monoid m)
    => (a -> m)
    -> Compose f g a
    -> m
  foldMap f (Compose a) = foldMap (foldMap (f :: a -> m) :: (g a -> m)) a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse ::
       forall m a b. Applicative m
    => (a -> m b)
    -> Compose f g a
    -> m (Compose f g b)
  traverse f1 (Compose ca) =
    let x1 = traverse (traverse f1) ca :: m (f (g b))
    in fmap Compose x1

main :: IO ()
main = do
  putStrLn "hello world"
