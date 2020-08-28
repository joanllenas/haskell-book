## Exercises: Lookups (pg.689)

In the following exercises, you will need to use the given terms to make the expressions type check:

1. `pure`
2. `(<$>)` _or `fmap`_
2. `(<*>)`

### 1.
```hs
added :: Maybe Integer 
added =
  (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
```

---

```hs
added :: Maybe Integer 
added =
  (+3) <$> (lookup (3::Integer) $ zip [1, 2, 3] [4, 5, 6])
```


### 2. 
```hs
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6] 
z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6] 
tupled :: Maybe (Integer, Integer)
tupled = (,) y z
```
---

```hs
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
```

### 3. 
```hs
import Data.List (elemIndex)
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]
y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]
max' :: Int -> Int -> Int 
max' = max
maxed :: Maybe Int 
maxed = max' x y
```

---

```hs
maxed :: Maybe Int 
maxed = max' <$> x <*> y
```


### 4. 
```hs
xs = [1, 2, 3] 
ys = [4, 5, 6]
x :: Maybe Integer
x = lookup 3 $ zip xs ys
y :: Maybe Integer
y = lookup 2 $ zip xs ys
summed :: Maybe Integer 
summed = sum $ (,) x y
```

---

```hs
summed :: Maybe Integer 
summed = fmap sum $ (,) <$> x <*> y
```



## Exercise: Identity instance (pg.692)

Write an `Applicative` instance for `Identity`:
```hs
newtype Identity a 
  = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined
instance Applicative Identity where 
  pure = undefined
  (<*>) = undefined
```

---

```hs
instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where 
  pure = Identity
  Identity f <*> Identity x = Identity (f x)
```



## Exercise: Constant instance (pg.693)

Write an `Applicative` instance for `Constant`:
```hs
newtype Constant a b 
  = Constant { getConstant :: a } 
  deriving (Eq, Ord, Show)
instance Functor (Constant a) where 
  fmap = undefined
instance Monoid a => Applicative (Constant a) where
  pure = undefined
  (<*>) = undefined
instance Applicative (Constant a) where 
  pure = undefined
  (<*>) = undefined
```

---

```hs
instance Functor (Constant a) where 
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant $ mappend x y
```



## Exercise: Fixer upper (pg.707)
Given the functions and values provided, use `<$>` from `Functor` and `<*>` and `pure` from the `Applicative` type class to fill in missing bits of the broken code below to make it work.

### 1.
`const <$> Just "Hello" <*> "World"`

---

`const <$> Just "Hello" <*> pure "World"`

### 2.
`(,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]`

---

```hs
x = (,,,) 
  <$> Just 90 
  <*> Just 10 
  <*> Just "Tierness" 
  <*> pure [1, 2, 3]
```



## List Applicative exercise (pg.719)
Implement `Applicative` for lists. Writing a minimally complete `Applicative` instance calls for writing the definitions of both `pure` and `<*>`. Use the `checkers` library to validate your `Applicative` instance:

---

```hs
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

instance Arbitrary a => Arbitrary (List a)  where
  arbitrary = sized go
    where go 0 = pure Nil
          go n = do
            xs <- go (n - 1)
            x  <- arbitrary
            return (Cons x xs)

instance (Eq a) => EqProp (List a) where 
  (=-=) = eq


mainList :: IO ()
mainList =
  hspec $ do
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Applicative" $
          (quickBatch $ applicative (undefined :: List (Int, Float, String)))
```




## ZipList Applicative exercise (pg.721)
Implement the `ZipList` `Applicative`:

---

```hs
newtype ZipList' a 
  = ZipList' [a] 
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 3000 l
          ys' = let (ZipList' l) = ys 
                in take 3000 l

instance Functor ZipList' where 
  fmap f (ZipList' xs) = ZipList' $ map f xs

instance Applicative ZipList' where 
  pure x = ZipList' $ repeat x
  ZipList' fs <*> ZipList' xs = ZipList' $ zipWith (\f x -> f x) fs xs

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = do
    xs <- arbitrary
    return $ ZipList' xs

mainZipList :: IO ()
mainZipList =
  hspec $ do
    describe "ZipList" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: ZipList' (Int, Float, String)))
      it "Applicative" $
          (quickBatch $ applicative (undefined :: ZipList' (Int, Float, String)))
```



## Exercise: Variations on Either (pg.725)
Implement the `Validation` `Applicative`:

```hs
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
      [ (1, return $ Success a) 
      , (2, return $ Failure e) 
      ]

mainValidation :: IO ()
mainValidation =
  hspec $ do
    describe "Validation" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Validation String (Int, Float, String)))
      it "Applicative" $
          (quickBatch $ applicative (undefined :: Validation String (Int, Float, String)))

```



## 17.9 Chapter exercises (pg.725)
Given a type that has an instance of `Applicative`, specialize the types of the methods.

### 1.
```hs
-- Type []
pure :: a -> [a]
pure = undefined
(<*>) :: [(a -> b)] -> [a] -> [b]
(<*>) = undefined
```

### 2.
```hs
-- Type IO
pure :: a -> IO a
pure = undefined
(<*>) :: IO (a -> b) -> IO a -> IO b
(<*>) = undefined
```

### 3.
```hs
-- Type (,) a
pure :: a -> (a,)
pure = undefined
(<*>) :: ((a -> b),) -> (a,) -> (b,)
(<*>) = undefined
```

### 4.
```hs
-- Type (->) e
???
```



## Write instances for the following datatypes. (pg.726)
Confused? Write out what the types should be. Use the checkers library to validate the instances:

### 1.
```hs
data Pair a 
  = Pair a a 
  deriving Show
```

---

```hs
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
      it "Functor" $
        property (quickBatch $ functor (undefined :: Pair (Int, Float, String)))
      it "Applicative" $
          (quickBatch $ applicative (undefined :: Pair (Int, Float, String)))

```

### 2.
```hs
data Two a b 
  = Two a b
  deriving Show
```

---

```hs
data Two a b 
  = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty
  Two a fb <*> Two a' b = Two (a <> a') (fb b) -- I cheated on this one. I had: Two a fb <*> Two a' b = Two a' (fb b). But in order to keep the "interchange:" law we must mappend a and a'. (I think that is the reason)

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
```

### 3.
```hs
data Three a b c 
  = Three a b c
```

---

```hs
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
```

### 4.
```hs
data Three' a b 
  = Three' a b b
```

---

```hs
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
```

### 5.
```hs
data Four a b c d 
  = Four a b c d
```

---

```hs
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
```

### 6.
```hs
data Four' a b 
  = Four' a a a b
```

---

```hs
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
```



## Combinations (pg.727)

```hs
stops :: String
stops = "pbtdkg" 
vowels :: String
vowels = "aeiou"
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = undefined
```

---

```hs
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\a b c -> (a, b, c))
```
