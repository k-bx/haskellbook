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

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b =
  Deux a
       b

instance Bifunctor Deux where
  bimap fa fb (Deux a b) = Deux (fa a) (fb b)

data Const' a b =
  Const' a

instance Bifunctor Const' where
  bimap fa fb (Const' a) = Const' (fa a)

data Drei a b c =
  Drei a
       b
       c

instance Bifunctor (Drei a) where
  bimap fb fc (Drei a b c) = Drei a (fb b) (fc c)

data SuperDrei a b c =
  SuperDrei a
            b

instance Bifunctor (SuperDrei a) where
  bimap fb fc (SuperDrei a b) = SuperDrei a (fb b)

data SemiDrei a b c =
  SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap fb fc (SemiDrei a) = SemiDrei a

data Quadriceps a b c d =
  Quadzzz a
          b
          c
          d

instance Bifunctor (Quadriceps a b) where
  bimap fc fd (Quadzzz a b c d) = Quadzzz a b (fc c) (fd d)

data Either' a b
  = Left' a
  | Right' b

instance Bifunctor Either' where
  bimap fa fb (Left' a) = Left' (fa a)
  bimap fa fb (Right' b) = Right' (fb b)

newtype IdentityT f a = IdentityT
  { runIdentityT :: f a
  } deriving (Eq, Show)

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative f => Applicative (IdentityT f) where
  pure = IdentityT . pure
  (<*>) :: forall a b. IdentityT f (a -> b) -> IdentityT f a -> IdentityT f b
  IdentityT f <*> IdentityT a =
    let f' = f :: f (a -> b)
        a' = a :: f a
        x' = f <*> a :: f b
    in IdentityT (f <*> a)

instance Monad f => Monad (IdentityT f) where
  return = pure
  (>>=) :: forall a b. IdentityT f a -> (a -> IdentityT f b) -> IdentityT f b
  IdentityT a >>= f =
    let a' = a :: f a
        f' = f :: a -> IdentityT f b
        h2 = runIdentityT . f :: a -> f b
        h3 = a >>= (runIdentityT . f) :: f b
    in IdentityT h3

main :: IO ()
main = do
  putStrLn "hello world"
