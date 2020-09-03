## The answer is the exercise
Write `bind` in terms of `fmap` and `join`.
```hs
-- keep in mind this is >>= flipped
bind :: Monad m => (a -> m b) -> m a -> m b bind = undefined
```

---

```hs
bind :: Monad m => (a -> m b) -> m a -> m b 
bind f m = join $ fmap f m
{-
λ> bind (\x -> pure $ x * 2) (Just 3)
Just 6
λ> bind (\x -> pure $ x * 2) Nothing
Nothing
-}
```



## Short Exercise: Either Monad (pg.759)
Implement the Either Monad:
```hs
data Sum a b 
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = undefined
instance Applicative (Sum a) where
  pure = undefined
  (<*>) = undefined
instance Monad (Sum a) where 
  return = pure
  (>>=) = undefined
  ```

  ---

  ```hs
  instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap _ (First a) = First a

instance (Monoid a) => Applicative (Sum a) where
  pure = Second
  First a <*> First a' = First $ a <> a'
  Second f <*> Second b = Second $ f b
  First a <*> Second _ = First a
  Second _ <*> First a = First a

instance (Monoid a) => Monad (Sum a) where 
  return = pure
  Second b >>= f = f b
  First a >>= _ = First a
  ```



## 18.7 Chapter exercises (pg.770)
Write `Monad` instances for the following types.

### 1. 
Welcome to the Nope `Monad`, where nothing happens and nobody cares:

```hs
data Nope a = NopeDotJpg
```

---

```hs
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
```

### 2.

```hs
data BahEither b a 
  = PLeft a
  | PRight b
```

---

```hs

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
```

### 3.
```hs
newtype Identity a 
  = Identity a 
  deriving (Eq, Ord, Show)
instance Functor Identity where 
  fmap = undefined
instance Applicative Identity where 
  pure = undefined
  (<*>) = undefined
instance Monad Identity where 
  return = pure
  (>>=) = undefined
```

---

```hs
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
```

### 4.
```hs
data List a 
  = Nil
  | Cons a (List a)
```

---

```hs
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
```



## Write the following functions using the methods provided by Monad and Functor. (pg.771)
Using stuff like identity and composition is fine, but it has to type check with the types provided:

### 1.
```hs
j :: Monad m => m (m a) -> m a 
```
Expecting the following behavior:
```
     Prelude> j [[1, 2], [], [3]]
     [1,2,3]
     Prelude> j (Just (Just 1))
     Just 1
     Prelude> j (Just Nothing)
     Nothing
     Prelude> j Nothing
     Nothing
```

---

```hs
j :: Monad m => m (m a) -> m a
j mma = join mma
```

### 2.
```hs
l1 :: Monad m => (a -> b) -> m a -> m b
```

---

```hs
l1 :: Monad m => (a -> b) -> m a -> m b
l1 a2b ma = fmap a2b ma
-- λ> l1 (+1) (Just 1)
-- Just 2
```

### 3.
```hs
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
```

---

```hs
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 a2b2c ma mb = a2b2c <$> ma <*> mb
-- λ> l2 (,) [1,2] [3,4]
-- [(1,3),(1,4),(2,3),(2,4)]
```

### 4.
```hs
a :: Monad m => m a -> m (a -> b) -> m b
```

---

```hs
a :: Monad m => m a -> m (a -> b) -> m b
a ma a2b = id <$> a2b <*> ma
-- λ> a (Just 1) (Just (\x -> x+1))
-- Just 2
```

### 5.
```hs
meh :: Monad m => [a] -> (a -> m b) -> m [b]
```

---

```hs
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xa a2mb = sequence $ map a2mb xa
-- λ> meh [1,2,3] (\n -> Just (n*2))
-- Just [2,4,6]
```

### 6.
```hs
flipType :: (Monad m) => [m a] -> m [a]
```

---

```hs
flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas (fmap id)
-- λ> flipType [Just 1, Just 2]
-- Just [1,2]
-- λ> flipType [Just 1, Just 2, Nothing, Just 3]
-- Nothing
```
