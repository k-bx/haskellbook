#!/usr/bin/env stack
-- stack script --resolver lts-10.1
import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

main :: IO ()
main = do
  quickCheck (\x -> functorIdentity (x :: [Int]))
  quickCheck (\(Fun _ f) (Fun _ g) x -> functorCompose (f::Int->String) (g::String->Int) (x::[Int]))
