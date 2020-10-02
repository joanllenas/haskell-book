{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type CheckersType = (Int, Float, String)

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- Chapter exercises (pg.839)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  Identity
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype Identity a
  = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldr ab2b b (Identity a) = ab2b a b

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- traverse :: (Int -> Maybe Int) -> Identity Int -> Maybe (Identity Int)
instance Traversable Identity where
  traverse a2fb ta@(Identity a) =
    let fb = a2fb a -- Maybe Int
     in fmap Identity fb -- Maybe (Identity Int)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

mainIdentity :: IO ()
mainIdentity =
  hspec $ do
    describe "Identity" $ do
      it "Traverse" $
        (quickBatch $ traversable (undefined :: Identity CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  Constant
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype Constant a b = Constant {getConstant :: a}
  deriving (Show, Eq)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant a) (Constant a') = Constant a

instance Foldable (Constant a) where
  foldr ab2b b ta@(Constant a) = b

instance Traversable (Constant z) where
  traverse _ (Constant x) = pure (Constant x)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

mainConstant :: IO ()
mainConstant =
  hspec $ do
    describe "Constant" $ do
      it "Traverse" $
        (quickBatch $ traversable (undefined :: Constant CheckersType CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  Maybe
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Applicative Optional where
  pure = Yep
  Yep f <*> Yep x = Yep $ f x
  Nada <*> _ = Nada
  _ <*> Nada = Nada

instance Foldable Optional where
  foldMap a2m (Yep a) = a2m a
  foldMap _ Nada = mempty

instance Traversable Optional where
  traverse a2fb (Yep a) = fmap Yep $ a2fb a
  traverse _ Nada = pure Nada

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency
      [ (1, return $ Yep a),
        (1, return $ Nada)
      ]

mainOptional :: IO ()
mainOptional =
  hspec $ do
    describe "Optional" $ do
      it "Traverse" $
        (quickBatch $ traversable (undefined :: Optional CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  List
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) $ fmap f la

instance Applicative List where
  pure = flip Cons Nil
  Cons f lf <*> Cons a la = Cons (f a) $ lf <*> la
  Nil <*> _ = Nil
  _ <*> Nil = Nil

instance Foldable List where
  foldMap a2m (Cons a la) = a2m a <> foldMap a2m la
  foldMap a2m Nil = mempty

-- traverse :: (Int -> Maybe Int) -> List Int -> Maybe (List Int)
instance Traversable List where
  traverse f (Cons a la) =
    let fb = f a
     in (fmap Cons fb) <*> traverse f la -- OMG
  traverse f Nil = pure Nil

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

{-
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]
-}

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    la <- arbitrary
    frequency
      [ (1, return $ Nil),
        (1, return $ Cons a la)
      ]

mainList :: IO ()
mainList =
  hspec $ do
    describe "List" $ do
      it "Traverse" $
        (quickBatch $ traversable (undefined :: List CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  Three
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b fc <*> Three a' b' c = Three (a <> a') (b <> b') $ fc c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-- traverse :: (Int -> Maybe Int) -> Three a b Int -> Maybe (Three a b Int)
instance Traversable (Three a b) where
  traverse f (Three a b c) = fmap (Three a b) $ f c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

mainThree :: IO ()
mainThree =
  hspec $ do
    describe "Three" $ do
      it "Traverse" $
        (quickBatch $ traversable (undefined :: Three CheckersType CheckersType CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  Pair
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Pair a b
  = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance (Monoid a) => Applicative (Pair a) where
  pure = Pair mempty
  Pair a fb <*> Pair a' b = Pair (a <> a') $ fb b

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = fmap (Pair a) $ f b

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

mainPair :: IO ()
mainPair =
  hspec $ do
    describe "Pair" $ do
      it "Traverse" $
        (quickBatch $ traversable (undefined :: Pair CheckersType CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  Big
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Big a b
  = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance (Semigroup a, Semigroup b) => Semigroup (Big a b) where
  Big a b c <> Big a' b' c' = Big (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b) => Monoid (Big a b) where
  mempty = Big mempty mempty mempty
  mappend = (<>)

instance (Monoid a) => Applicative (Big a) where
  pure b = Big mempty b b
  Big a fb fb' <*> Big a' b b' = Big (a <> a') (fb b) (fb' b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'

-- traverse :: (Int -> Maybe Int) -> Big _ Int -> Maybe (Big _ Int)
instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

--traverse f (Big x y z) = liftA2 (Big x) (f y) (f z)

-- traverse Just (Big "Hello" 1 2)
-- (Big "Hello") <$> Just 1 <*> Just 2
-- Just (Big "Hello" 1) <*> Just 2
-- Just (Big "Hello" 1 2)

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Big a b b'

mainBig :: IO ()
mainBig =
  hspec $ do
    describe "Big" $ do
      it "Functor" $
        (quickBatch $ functor (undefined :: Big (Sum Int) CheckersType))
      it "Monoid" $
        (quickBatch $ monoid (undefined :: Big (Sum Int) (Product Int)))
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Big (Sum Int) CheckersType))
      it "Foldable" $
        (quickBatch $ foldable (undefined :: Big (Sum Int) (Sum Int, Sum Int, Any, Sum Int, String)))
      it "Traversable" $
        (quickBatch $ traversable (undefined :: Big (Sum Int) CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  Bigger
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Bigger a b
  = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance (Semigroup a, Semigroup b) => Semigroup (Bigger a b) where
  Bigger a b c d <> Bigger a' b' c' d' = Bigger (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b) => Monoid (Bigger a b) where
  mempty = Bigger mempty mempty mempty mempty
  mappend = (<>)

instance (Monoid a) => Applicative (Bigger a) where
  pure b = Bigger mempty b b b
  Bigger a fb fb' fb'' <*> Bigger a' b b' b'' = Bigger (a <> a') (fb b) (fb' b') (fb'' b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = (Bigger a) <$> f b <*> f c <*> f d

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Bigger a b c d

mainBigger :: IO ()
mainBigger =
  hspec $ do
    describe "Bigger" $ do
      it "Functor" $
        (quickBatch $ functor (undefined :: Bigger (Sum Int) CheckersType))
      it "Monoid" $
        (quickBatch $ monoid (undefined :: Bigger (Sum Int) (Product Int)))
      it "Applicative" $
        (quickBatch $ applicative (undefined :: Bigger (Sum Int) CheckersType))
      it "Foldable" $
        (quickBatch $ foldable (undefined :: Bigger (Sum Int) (Sum Int, Sum Int, Any, Sum Int, String)))
      it "Traversable" $
        (quickBatch $ traversable (undefined :: Bigger (Sum Int) CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  S
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data S n a
  = S (n a) a
  deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

--   foldr f x (S na a) = f a (foldr f x na)

instance (Traversable n) => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

-- S [] Int (or) S Maybe Int
-- traverse Just (S [1,2] 3)
-- S <$> Just [1,2] <*> Just 3
-- Just S [1,2] <*> Just 3
-- Just (S [1,2] 3)

instance
  ( Functor n,
    Arbitrary (n a),
    Arbitrary a
  ) =>
  Arbitrary (S n a)
  where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

mainS :: IO ()
mainS =
  hspec $ do
    describe "S" $ do
      it "Functor" $
        (quickBatch $ functor (undefined :: S [] CheckersType))
      it "Foldable" $
        (quickBatch $ foldable (undefined :: S [] (Sum Int, Sum Int, Any, Sum Int, String)))
      it "Traversable" $
        (quickBatch $ traversable (undefined :: S [] CheckersType))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--  Tree
--
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node lt a rt) = Node (fmap f lt) (f a) (fmap f rt)

instance Foldable Tree where
  {-
  Andrew
    foldr _ x Empty    = x
    foldr f x (Leaf a) = f a x
    foldr f x (Node ls a rs) =
      let acc1 = f a x
          acc2 = foldr f acc1 ls
      in  foldr f acc2 rs
  -}
  {-
  Maxim
  foldr f b (Node t1 a t2) = foldr f (f a $ foldr f b t2) t1
  -}
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node lt a rt) = foldMap f lt <> f a <> foldMap f rt

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node ls x rs) = Node <$> traverse f ls <*> f x <*> traverse f rs

--traverse f (Node xs y ys) = liftA3 Node (traverse f xs) (f y) (traverse f ys)

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    lt <- arbitrary
    rt <- arbitrary
    frequency
      [ (1, return $ Empty),
        (3, return $ (Leaf a1)),
        (2, return $ (Node lt a2 rt))
      ]

mainTree :: IO ()
mainTree =
  hspec $ do
    describe "Tree" $ do
      it "Functor" $
        (quickBatch $ functor (undefined :: Tree CheckersType))
      it "Foldable" $
        (quickBatch $ foldable (undefined :: Tree (Sum Int, Sum Int, Any, Sum Int, String)))
      it "Traversable" $
        (quickBatch $ traversable (undefined :: Tree CheckersType))

-- ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
--
--  Main
--
-- ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

main = do
  mainIdentity
  mainConstant
  mainOptional
  mainList
  mainThree
  mainPair
  mainBig
  mainBigger
  mainS
  mainTree
