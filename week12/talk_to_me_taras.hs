#!/usr/bin/env stack
-- stack script --resolver lts-10.1
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Funct where

import Data.Data

-- import           Data.Typeable
import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1
data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
newtype K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

genFlipK :: (Arbitrary b) => Gen (Flip K a b)
genFlipK = do
  b <- arbitrary
  return (Flip (K b))

instance (Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = genFlipK

functorIdentityFlipK :: Flip K Int String -> Bool
functorIdentityFlipK = functorIdentity :: Flip K Int String -> Bool

functorComposeFlipK ::
     Flip K Int String -> Fun String Int -> Fun Int Double -> Bool
functorComposeFlipK =
  functorCompose' :: Flip K Int String -> Fun String Int -> Fun Int Double -> Bool

quickCheckFlipK :: IO ()
quickCheckFlipK = do
  quickCheck functorIdentityFlipK
  quickCheck functorComposeFlipK

-- 4
newtype EvilGoateeConst a b =
  GoatyConst b
  deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

genEvilGoateeConst :: (Arbitrary b) => Gen (EvilGoateeConst a b)
genEvilGoateeConst = do
  b <- arbitrary
  return (GoatyConst b)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = genEvilGoateeConst

functorIdentityGoatyConst :: EvilGoateeConst Int String -> Bool
functorIdentityGoatyConst =
  functorIdentity :: EvilGoateeConst Int String -> Bool

functorComposeGoatyConst ::
     EvilGoateeConst Int String -> Fun String Int -> Fun Int Double -> Bool
functorComposeGoatyConst =
  functorCompose' :: EvilGoateeConst Int String -> Fun String Int -> Fun Int Double -> Bool

quickCheckGoatyConst :: IO ()
quickCheckGoatyConst = do
  quickCheck functorIdentityGoatyConst
  quickCheck functorComposeGoatyConst

-- 5
newtype LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

genLiftItOut :: (Arbitrary (f a)) => Gen (LiftItOut f a)
genLiftItOut = do
  fa <- arbitrary
  return (LiftItOut fa)

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = genLiftItOut

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

functorComposeLiftItOut ::
     LiftItOut [] String -> Fun String Int -> Fun Int Double -> Bool
functorComposeLiftItOut =
  functorCompose' :: LiftItOut [] String -> Fun String Int -> Fun Int Double -> Bool

functorIdentityLiftItOut :: LiftItOut [] String -> Bool
functorIdentityLiftItOut = functorIdentity :: LiftItOut [] String -> Bool

quickCheckLiftItOut :: IO ()
quickCheckLiftItOut = do
  quickCheck functorIdentityLiftItOut
  quickCheck functorComposeLiftItOut

-- 6.
data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

genParappa :: (Arbitrary (f a), Arbitrary (g a)) => Gen (Parappa f g a)
genParappa = do
  fa <- arbitrary
  ga <- arbitrary
  return (DaWrappa fa ga)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = genParappa

functorIdentityParappa :: Parappa [] Maybe String -> Bool
functorIdentityParappa = functorIdentity :: Parappa [] Maybe String -> Bool

functorComposeParappa ::
     Parappa [] Maybe String -> Fun String Int -> Fun Int Double -> Bool
functorComposeParappa =
  functorCompose' :: Parappa [] Maybe String -> Fun String Int -> Fun Int Double -> Bool

quickCheckParappa :: IO ()
quickCheckParappa = do
  quickCheck functorIdentityParappa
  quickCheck functorComposeParappa

-- 7.
data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)

genIgnoreOne :: (Arbitrary (f a), Arbitrary (g b)) => Gen (IgnoreOne f g a b)
genIgnoreOne = do
  fa <- arbitrary
  gb <- arbitrary
  return (IgnoringSomething fa gb)

instance (Arbitrary (f a), Arbitrary (g b)) =>
         Arbitrary (IgnoreOne f g a b) where
  arbitrary = genIgnoreOne

functorIdentityIgnoreOne :: IgnoreOne [] Maybe Int Int -> Bool
functorIdentityIgnoreOne = functorIdentity :: IgnoreOne [] Maybe Int Int -> Bool

functorComposeIgnoreOne ::
     IgnoreOne [] Maybe Int Int -> Fun Int Int -> Fun Int Double -> Bool
functorComposeIgnoreOne =
  functorCompose' :: IgnoreOne [] Maybe Int Int -> Fun Int Int -> Fun Int Double -> Bool

quickCheckIgnoreOne :: IO ()
quickCheckIgnoreOne = do
  quickCheck functorIdentityIgnoreOne
  quickCheck functorComposeIgnoreOne

-- 8.
data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap h (Notorious ga go gt) = Notorious ga go (fmap h gt)

genNotorious ::
     (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t))
  => Gen (Notorious g o a t)
genNotorious = do
  go <- arbitrary
  ga <- arbitrary
  gt <- arbitrary
  return (Notorious go ga gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) =>
         Arbitrary (Notorious g o a t) where
  arbitrary = genNotorious

functorIdentityNotorious :: Notorious [] Int Bool String -> Bool
functorIdentityNotorious =
  functorIdentity :: Notorious [] Int Bool String -> Bool

functorComposeNotorious ::
     Notorious [] Int Bool String -> Fun String Int -> Fun Int Double -> Bool
functorComposeNotorious =
  functorCompose' :: Notorious [] Int Bool String -> Fun String Int -> Fun Int Double -> Bool

quickCheckNotorious :: IO ()
quickCheckNotorious = do
  quickCheck functorIdentityNotorious
  quickCheck functorComposeNotorious

-- 9.
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

genSizedList :: Arbitrary a => Int -> Gen (List a)
genSizedList m = do
  n <- choose (0, m)
  a <- arbitrary
  case n of
    0 -> return Nil
    1 -> return (Cons a Nil)
    _ -> do
      t <- genSizedList (n - 1)
      return (Cons a t)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized genSizedList

functorIdentityList :: List String -> Bool
functorIdentityList = functorIdentity :: List String -> Bool

functorComposeList :: List String -> Fun String Int -> Fun Int Double -> Bool
functorComposeList =
  functorCompose' :: List String -> Fun String Int -> Fun Int Double -> Bool

quickCheckList :: IO ()
quickCheckList = do
  quickCheck functorIdentityList
  quickCheck functorComposeList

-- 10.
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

genGoatLordSized :: Arbitrary a => Int -> Gen (GoatLord a)
genGoatLordSized m = do
  n <- choose (0, m)
  a <- arbitrary
  case n of
    0 -> return NoGoat
    1 -> frequency [(1, return NoGoat), (2, return (OneGoat a))]
    2 -> frequency [(1, return NoGoat), (2, return (OneGoat a))]
    _ -> do
      ga <- genGoatLordSized (n - 1)
      gb <- genGoatLordSized (n - 1)
      gc <- genGoatLordSized (n - 1)
      frequency
        [ (1, return NoGoat)
        , (2, return (OneGoat a))
        , (5, return (MoreGoats ga gb gc))
        ]

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = sized genGoatLordSized

functorIdentityGoatLord :: GoatLord Int -> Bool
functorIdentityGoatLord = functorIdentity :: GoatLord Int -> Bool

functorComposeGoatLord :: GoatLord Int -> Fun Int Int -> Fun Int Int -> Bool
functorComposeGoatLord =
  functorCompose' :: GoatLord Int -> Fun Int Int -> Fun Int Int -> Bool

quickCheckGoatLord :: IO ()
quickCheckGoatLord = do
  quickCheck functorIdentityGoatLord
  quickCheck functorComposeGoatLord

-- 11
data TalkToMe a b
  = Halt
  | Print a
          b
  | Read { runF :: a -> b }

instance (Show a, Show b, Typeable a, Typeable b) => Show (TalkToMe a b) where
  show Halt = "Halt"
  show (Print x y) = "Print " ++ show x ++ " " ++ show y
  show (Read f) = "Read " ++ show (typeOf f)

instance (Eq a, Eq b) => Eq (TalkToMe a b) where
  Halt == Halt = True
  Halt == _ = False
  _ == Halt = False
  (Print x y) == (Print x' y') = x == x' && y == y'
  (Print _ _) == _ = False
  _ == (Print _ _) = False
  (Read _) == (Read _) = undefined
  --(Read f) == (Read g) = show (typeOf f) == show (typeOf g)

instance Functor (TalkToMe a) where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read fs) = Read (f . fs)

genTalkToMe ::
     (Function a, CoArbitrary a, Arbitrary b, Arbitrary a) => Gen (TalkToMe a b)
genTalkToMe = do
  func <- arbitrary
  x <- arbitrary
  str <- arbitrary
  frequency
    [(1, return Halt), (2, return (Print str x)), (2, return (Read func))]

instance (Function a, CoArbitrary a, Arbitrary b, Arbitrary a) =>
         Arbitrary (TalkToMe a b) where
  arbitrary = genTalkToMe

functorIdentityTalkToMe :: String -> TalkToMe String Int -> Bool
functorIdentityTalkToMe x (Read fsi) =
  runF (fmap id (Read fsi)) x == runF (Read fsi) x
functorIdentityTalkToMe _ x = (fmap id x) == x

functorComposeTalkToMe ::
     String -> TalkToMe String Int -> Fun Int Double -> Fun Double Char -> Bool
functorComposeTalkToMe s x@(Read _) (Fun _ f) (Fun _ g) =
  runF (fmap (g . f) x) s == runF (fmap g . fmap f $ x) s
functorComposeTalkToMe _ x (Fun _ f) (Fun _ g) =
  fmap (g . f) x == (fmap g . fmap f $ x)

quickCheckTalkToMe :: IO ()
quickCheckTalkToMe = do
  quickCheck functorIdentityTalkToMe
  quickCheck functorComposeTalkToMe

main :: IO ()
main = do
  -- quickCheckFlipK
  -- quickCheckGoatyConst
  -- quickCheckLiftItOut
  -- quickCheckParappa
  -- quickCheckIgnoreOne
  -- quickCheckNotorious
  -- quickCheckList
  -- quickCheckGoatLord
  quickCheckTalkToMe
