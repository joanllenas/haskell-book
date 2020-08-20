module Main where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

fArrInt :: [Int] -> Bool
fArrInt x = functorIdentity x


----------------------------------------
--
--  16.10 Exercises: Instances of Func (pg.651)
--
----------------------------------------

{-
1. `newtype Identity a = Identity a`
2. `data Pair a = Pair a a`
3. `data Two a b = Two a b`
4. `data Three a b c = Three a b c`
5. `data Three' a b = Three' a b b`
6. `data Four a b c d = Four a b c d`
7. `data Four' a b = Four' a a a b`
-}

----------------------------------------
--
--  1.
--
----------------------------------------

newtype Identity a 
  = Identity a 
  deriving (Show, Eq)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do 
  x <- arbitrary
  return $ Identity x
  
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = genIdentity

identityId :: Identity String -> Bool
identityId x = functorIdentity x

identityComp :: Identity String -> Bool
identityComp x = functorCompose (++"_a_") reverse x

----------------------------------------
--
--  2.
--
----------------------------------------

data Pair a 
  = Pair a a
  deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  x <- arbitrary
  y <- arbitrary
  return $ Pair x y

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = pairGen

pairId :: Pair String -> Bool
pairId x = functorIdentity x

pairComp :: Pair String -> Bool
pairComp x = functorCompose (++"_b_") reverse x

----------------------------------------
--
--  3.
--
----------------------------------------

data Two a b 
  = Two a b
  deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoId :: Two Int String -> Bool
twoId x = functorIdentity x

twoComp :: Two Int String -> Bool
twoComp x = functorCompose (++"_b_") reverse x

----------------------------------------
--
--  4.
--
----------------------------------------

data Three a b c 
  = Three a b c
  deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

threeId :: Three Int Int String -> Bool
threeId x = functorIdentity x

threeComp :: Three Int Int String -> Bool
threeComp x = functorCompose (++"_b_") reverse x

----------------------------------------
--
--  5.
--
----------------------------------------

data Three' a b 
  = Three' a b b
  deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

threeGen' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threeGen' = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three' x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = threeGen'

threeId' :: Three' Int String -> Bool
threeId' x = functorIdentity x

threeComp' :: Three' Int String -> Bool
threeComp' x = functorCompose (++"_b_") reverse x

----------------------------------------
--
--  6.
--
----------------------------------------

data Four a b c d
  = Four a b c d
  deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

fourId :: Four Int Int Int String -> Bool
fourId x = functorIdentity x

fourComp :: Four Int Int Int String -> Bool
fourComp x = functorCompose (++"_b_") reverse x

----------------------------------------
--
--  7.
--
----------------------------------------

data Four' a b 
  = Four' a a a b
  deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourGen' = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four' a b c d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourGen'

fourId' :: Four' Int String -> Bool
fourId' x = functorIdentity x

fourComp' :: Four' Int String -> Bool
fourComp' x = functorCompose (++"_b_") reverse x

----------------------------------------
--
--  8.
--
----------------------------------------

-- Trivial can't be a Functor becasue its kind is `*` and Functors must be `* -> *`.

data Trivial
  = Trivial
  deriving (Show, Eq)
-- instance Functor Trivial where
--   fmap f t = Trivial



----------------------------------------
--
--  Exercise: Possibly (pg.654)
--
----------------------------------------

{-
Write a Functor instance for a datatype identical to Maybe. We’ll use our own datatype, because Maybe already has a Functor instance and we cannot make a duplicate one.

```hs
data Possibly a 
  = LolNope
  | Yeppers a 
  deriving (Eq, Show)
instance Functor Possibly where
  fmap = undefined
```
-}

data Possibly a 
  = LolNope
  | Yeppers a 
  deriving (Eq, Show)
instance Functor Possibly where
  fmap f (Yeppers x) = Yeppers (f x)
  fmap _ LolNope = LolNope

genPossibly :: Arbitrary a => Gen (Possibly a)
genPossibly = do 
  x <- arbitrary
  frequency 
    [ (1, return $ LolNope) 
    , (1, return $ Yeppers x) 
    ]
  
instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = genPossibly

possiblyId :: Possibly String -> Bool
possiblyId x = functorIdentity x

possiblyComp :: Possibly String -> Bool
possiblyComp x = functorCompose (++"_a_") reverse x



{-
## Short exercise (pg.657)
-}

----------------------------------------
--
--  1.
--
----------------------------------------

data Sum a b 
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

genSum :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
genSum = do 
  x <- arbitrary
  y <- arbitrary
  frequency 
    [ (1, return $ First x) 
    , (1, return $ Second y) 
    ]
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = genSum

sumId :: Sum Int String -> Bool
sumId x = functorIdentity x

sumComp :: Sum Int String -> Bool
sumComp x = functorCompose (++"_a_") reverse x


----------------------------------------
--
--  16.17 Chapter exercises (pg.667)
--
----------------------------------------

----------------------------------------
--
--  2.
--
----------------------------------------

data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)

----------------------------------------
--
--  3.
--
----------------------------------------

data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish = Falsish
  fmap f (Truish x) = Truish (f x)


----------------------------------------
--
--  Main
--
----------------------------------------

main :: IO ()
main = do
  quickCheck fArrInt
  quickCheck identityId
  quickCheck identityComp
  quickCheck pairId
  quickCheck pairComp
  quickCheck twoId
  quickCheck twoComp
  quickCheck threeId
  quickCheck threeComp
  quickCheck threeId'
  quickCheck threeComp'
  quickCheck fourId
  quickCheck fourComp
  quickCheck fourId'
  quickCheck fourComp'
  quickCheck possiblyId
  quickCheck possiblyComp
  quickCheck sumId
  quickCheck sumComp
