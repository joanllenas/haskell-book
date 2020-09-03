module Main where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- ## List Applicative exercise (pg.719)

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> as = fmap f as `append` (fs <*> as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized go
    where
      go 0 = pure Nil
      go n = do
        xs <- go (n - 1)
        x <- arbitrary
        return (Cons x xs)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

mainList :: IO ()
mainList =
  hspec $ do
    describe "List" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: List (Int, Float, String)))

-- ## ZipList Applicative exercise (pg.721)

newtype ZipList' a
  = ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take 3000 l
      ys' =
        let (ZipList' l) = ys
         in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat x
  ZipList' fs <*> ZipList' xs = ZipList' $ zipWith (\f x -> f x) fs xs

-- Uli: ZipList' fs <*> ZipList' ys = ZipList' (zipWith ($) fs ys)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = do
    xs <- arbitrary
    return $ ZipList' xs

mainZipList :: IO ()
mainZipList =
  hspec $ do
    describe "ZipList" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: ZipList' (Int, Float, String)))

-- ## Exercise: Variations on Either (pg.725)

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success a) = Success $ f a
  fmap f (Failure e) = Failure e

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure e <*> Failure e' = Failure $ e <> e'
  Success a <*> Failure e = Failure e
  Failure e <*> Success a = Failure e
  Success f <*> Success a = Success $ f a --  << not sure about this??

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency
      [ (1, return $ Success a),
        (2, return $ Failure e)
      ]

mainValidation :: IO ()
mainValidation =
  hspec $ do
    describe "Validation" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Validation String (Int, Float, String)))

-- ## Write instances for the following datatypes. (pg.726)

-- 1.
data Pair a
  = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  Pair fx fy <*> Pair x y = Pair (fx x) (fy y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

mainPair :: IO ()
mainPair =
  hspec $ do
    describe "Pair" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Pair (Int, Float, String)))

-- 2.
data Two a b
  = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty
  Two a fb <*> Two a' b = Two (a <> a') (fb b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

mainTwo :: IO ()
mainTwo =
  hspec $ do
    describe "Two" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Two String (Int, Float, String)))

-- 3.
data Three a b c
  = Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b fc <*> Three a' b' c = Three (a <> a') (b <> b') $ fc c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

mainThree :: IO ()
mainThree =
  hspec $ do
    describe "Three" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Three String String (Int, Float, String)))

-- 4.
data Three' a b
  = Three' a b b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) $ f b'

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a fx fy <*> Three' a' x y = Three' (a <> a') (fx x) $ fy y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

mainThree' :: IO ()
mainThree' =
  hspec $ do
    describe "Three'" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Three' String (Int, Float, String)))

-- 5.
data Four a b c d
  = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' x = Four (a <> a') (b <> b') (c <> c') $ f x

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

mainFour :: IO ()
mainFour =
  hspec $ do
    describe "Four" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Four String String String (Int, Float, String)))

-- 6.
data Four' a b
  = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' $ f b

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a1 a2 a3 f <*> Four' a1' a2' a3' x = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

mainFour' :: IO ()
mainFour' =
  hspec $ do
    describe "Four'" $ do
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Four' String (Int, Float, String)))

-- MAINS

main :: IO ()
main = do
  mainList
  mainZipList
  mainValidation
  mainPair
  mainTwo
  mainThree
  mainThree'
  mainFour
  mainFour'