#!/usr/bin/env stack
-- stack script --resolver lts-10.1
{-# LANGUAGE TypeApplications #-}

import Test.QuickCheck
import Test.QuickCheck.Function

data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

talkToMeAreEqual :: Eq a => String -> TalkToMe a -> TalkToMe a -> Bool
talkToMeAreEqual _ Halt Halt = True
talkToMeAreEqual _ (Print s1 x) (Print s2 y) = s1 == s2 && x == y
talkToMeAreEqual s (Read f1) (Read f2) = f1 s == f2 s

instance Show a => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print s a) = "Print " ++ show s ++ " " ++ show a
  show (Read _f) = "Read"

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read sf) = Read (f . sf)

genTalkToMe :: Arbitrary a => Gen (TalkToMe a)
genTalkToMe =
  frequency
    [ (1, pure Halt)
    , (1, fmap (\(s, a) -> Print s a) arbitrary)
    , (1, fmap Read arbitrary)
    ]

instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary = genTalkToMe

main :: IO ()
main = do
  quickCheck (functorIdentityTalkToMe @Int)
  quickCheck (functorComposeTalkToMe @Int @Int @Int)

functorIdentityTalkToMe :: Eq a => String -> TalkToMe a -> Bool
functorIdentityTalkToMe s f = talkToMeAreEqual s (fmap id f) f

functorComposeTalkToMe ::
     (Show a, Eq c) => String -> Fun a b -> Fun b c -> TalkToMe a -> Bool
functorComposeTalkToMe s (Fun _ f) (Fun _ g) x =
  talkToMeAreEqual s (fmap g (fmap f x)) (fmap (g . f) x)
