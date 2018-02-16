#!/usr/bin/env stack
-- stack script --resolver lts-10.1
{-# LANGUAGE ApplicativeDo #-}
import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats a1 a2 a3) = MoreGoats (fmap f a1) (fmap f a2) (fmap f a3)

genGoatLord :: Arbitrary a => Gen (GoatLord a)
genGoatLord = sized genGoatLordSized

genGoatLordSized :: Arbitrary a => Int -> Gen (GoatLord a)
genGoatLordSized 0 = pure NoGoat
genGoatLordSized n = do
  goRecurse <- arbitrary :: Gen Bool
  i <- choose (0, n `div` 3)
  j <- choose (0, n `div` 3)
  k <- choose (0, n `div` 3)
  if not goRecurse
    then frequency [(1, pure NoGoat), (2, fmap OneGoat arbitrary)]
    else do
      b1 <- genGoatLordSized i
      b2 <- genGoatLordSized j
      b3 <- genGoatLordSized k
      return (MoreGoats b1 b2 b3)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = genGoatLord

main :: IO ()
main = do
  quickCheck (\x -> functorIdentity (x :: GoatLord Int))
  quickCheck
    (\(Fun _ f) (Fun _ g) x ->
       functorCompose
         (f :: Int -> String)
         (g :: String -> Int)
         (x :: GoatLord Int))
