module Main where

import Control.Monad
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.

data Nope a
  = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

mainNope :: IO ()
mainNope =
  hspec $ do
    describe "Nope" $ do
      it "Monad" $
        (quickBatch $ monad (undefined :: Nope (Int, Float, String)))

-- 2.

data BahEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft a) = PLeft $ f a
  fmap f (PRight b) = PRight b

instance (Monoid b) => Applicative (BahEither b) where
  pure = PLeft
  PLeft f <*> PLeft a = PLeft $ f a
  PRight b <*> PRight b' = PRight $ b
  PLeft a <*> PRight b = PRight b
  PRight b <*> PLeft a = PRight b

instance (Monoid b) => Monad (BahEither b) where
  PLeft a >>= a2m = a2m a
  PRight b >>= _ = PRight b

instance (Eq b, Eq a) => EqProp (BahEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency
      [ (1, return $ PRight b),
        (1, return $ PLeft a)
      ]

mainBahEither :: IO ()
mainBahEither =
  hspec $ do
    describe "BahEither" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: BahEither String (Int, Float, String)))
      it "Monad" $
        (quickBatch $ monad (undefined :: BahEither String (Int, Float, String)))

-- 3.

newtype Identity a
  = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

mainIdentity :: IO ()
mainIdentity =
  hspec $ do
    describe "Identity" $ do
      it "Monad" $
        (quickBatch $ monad (undefined :: Identity (Int, Float, String)))

-- 4.

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> as = fmap f as `append` (fs <*> as)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized go
    where
      go 0 = pure Nil
      go n = do
        xs <- go (n - 1)
        x <- arbitrary
        return (Cons x xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = f x `append` (xs >>= f)

mainList :: IO ()
mainList =
  hspec $ do
    describe "List" $ do
      it "Monad" $
        (quickBatch $ monad (undefined :: List (Int, Float, String)))

-- 1

j :: Monad m => m (m a) -> m a
j mma = join mma

-- 2.

l1 :: Monad m => (a -> b) -> m a -> m b
l1 a2b ma = fmap a2b ma

-- 3.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 a2b2c ma mb = a2b2c <$> ma <*> mb

-- 4.

a :: Monad m => m a -> m (a -> b) -> m b
a ma a2b = id <$> a2b <*> ma

-- 5.

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xa a2mb = sequence $ map a2mb xa -- hoogle FTW

-- 6.

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas (fmap id)

-- MAINS

main :: IO ()
main = do
  mainNope
  mainBahEither
  mainIdentity
  mainList