## Exercises: Be kind

### 1.
What’s the kind of `a`?

`f :: a -> a; f = undefined`

---

- The kind of `a` is `*` because it's not applied to any arguments.

### 2. 
What are the kinds of `b` and `T`? (The `T` is capitalized on purpose!)

`f :: a -> b a -> T (b a)`

---

- The kind of `b` is `* -> *`, because `b` is a variable unary type constructor.
- The kind of `T` is `* -> *`.

### 3. 
What’s the kind of `c`?

`f :: c a b -> c b a`

---

- The kind of `c` is  `* -> * -> *` because it's a variable data constructor with two arguments.


## Exercises: Heavy lifting (pg.646)

Add `fmap`, parentheses, and function composition to each expression as needed for the expression to type check and produce the expected result.

### 1.
```hs
a = (+1) $ read "[1]" :: [Int]
```

Expected result:

```
Prelude> a
[2]
```

---

```hs
a = fmap (+1) $ read "[1]" :: [Int]
```

### 2.
```hs
b = (++ "lol") (Just ["Hi,", "Hello"])

-- Prelude> b
-- Just ["Hi,lol","Hellolol"]
```

---

```hs
b = (fmap .fmap)  (++ "lol") (Just ["Hi,", "Hello"])
```

### 3.
```hs
c = (*2) (\x -> x - 2)
-- Prelude> c 1
-- -2
```

---

```hs
c = (*2) . (\x -> x - 2)
```

### 4.
```hs
d = 
  ((return '1' ++) . show) 
  (\x -> [x, 1..3])
-- Prelude> d 0
-- "1[0,1,2,3]"
```

---

```hs
d 
  = ((return '1' ++) . show) 
  . (\x -> [x, 1..3])
```

### 5.
```hs
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123"++) show ioi 
    in (*3) changed
-- Prelude> e
-- 3693
```

---

```hs
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed
```



## 16.10 Exercises: Instances of Func (pg.651)

> See the `exercises` project

Implement Functor instances for the following datatypes. Use the QuickCheck properties we showed you to validate them:

1. `newtype Identity a = Identity a`
2. `data Pair a = Pair a a`
3. `data Two a b = Two a b`
4. `data Three a b c = Three a b c`
5. `data Three' a b = Three' a b b`
6. `data Four a b c d = Four a b c d`
7. `data Four' a b = Four' a a a b`

---

```hs
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

{-
import Test.QuickCheck.Function

functorCompose :: (Eq (f c), Functor f) => f a -> (Fun a b) -> (Fun b c)-> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

quickCheck (functorCompose :: (Identity Int) -> (Fun Int Int) -> (Fun Int Int) -> Bool)
-}

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
```

### 8.
`data Trivial = Trivial`

---

`Trivial` can't be implemented becasue its kind is `*` and Functors require `* -> *`.


## Exercise: Possibly (pg.654)
Write a Functor instance for a datatype identical to Maybe. We’ll use our own datatype, because Maybe already has a Functor instance and we cannot make a duplicate one.

```hs
data Possibly a 
  = LolNope
  | Yeppers a 
  deriving (Eq, Show)
instance Functor Possibly where
  fmap = undefined
```

---

```hs
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
```



## Short exercise (pg.657)

### 1.
Write a `Functor` instance for a datatype identical to `Either`. We’ll use our own datatype, because `Either` has a `Functor` instance:

```hs
data Sum a b 
  = First a
  | Second b
  deriving (Eq, Show)
instance Functor (Sum a) where
  fmap = undefined
```

---

```hs
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
```

### 2.

Why is a `Functor` instance that applies a function only to `First`, `Either’s` `Left`, impossible?

---

- Becasue to go from kind `* -> * -> *` to kind `* -> *` we have to partially apply the `Either a b` type with `Either a`. It's impossible to partially apply `Either`'s `a` becasue it's in the first position, and we can't use `flip` with types.



## 16.17 Chapter exercises (pg.667)

Determine whether a valid `Functor` can be written for the datatype provided:

### 1. 
`data Bool = False | True`

---

- No, because Bool's kind is `*`.

### 2. 
`data BoolAndSomethingElse a = False' a | True' a`

---

```hs
instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)
```

### 3. 
`data BoolAndMaybeSomethingElse a = Falsish | Truish a`

---

```hs
instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish = Falsish
  fmap f (Truish x) = Truish (f x)
```

### 4. 
Use the kinds to guide you on this one. Don’t get too hung up on the details:

`newtype Mu f = InF { outF :: f (Mu f) }`

---

- No, because its kind is `(* -> *) -> *`.

5. Again, follow the kinds, and ignore the unfamiliar parts:
```
import GHC.Arr
data D = D (Array Word Word) Int Int
```

---

- No, because its kind is `*`.



## More...


Rearrange the arguments to the type constructor of the datatype so the `Functor` instance works:

****
ALL WRONG!!! 👎

I did another thing different from what the exercise was asking.
****


### 1. 
```hs
data Sum a b 
  = First a
  | Second b

instance Functor (Sum e) where 
  fmap f (First a) = First (f a) 
  fmap f (Second b) = Second b
```

---

`b` is the mappable one, not `a`:

```hs
instance Functor (Sum a) where 
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
```

### 2. 
```hs
data Company a b c 
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where 
  fmap f (Something b) = Something (f b) 
  fmap _ (DeepBlue a c) = DeepBlue a c
```

---

`DeepBlue` is the mappable one (only the `c`), not `Something`:
```hs
instance Functor (Company a b) where 
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  fmap _ (Something b) = Something b
```

### 3. 
```hs
data More a b 
  = L a b a
  | R b a b 
  deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
```

---

`b` is the mappable one, not `a`:

```hs
instance Functor (More a) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')
```



## More...

Write Functor instances for the following datatypes:

### 1.
```hs
data Quant a b 
  = Finance
  | Desk a 
  | Bloor b
```

---

```hs
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)
```

### 2.
```hs
data K a b = K a
```

---

```hs
instance Functor (K a) where
  fmap _ (K a) = K a
```

### 3.
```hs
{-# LANGUAGE FlexibleInstances #-}

newtype Flip f a b 
  = Flip (f b a) 
  deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where
  fmap = undefined
```

---

```hs
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)
```

### 4.
```hs
data EvilGoateeConst a b = GoatyConst b
```

---

```hs
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b
```

### 5. 
Do you need something extra to make the instance work?

```hs
data LiftItOut f a = LiftItOut (f a)
```

---

We need to make sure that `f` is a `Functor`.

```hs
instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa
```

### 6.
```hs
data Parappa f g a 
  = DaWrappa (f a) (g a)
```

---

```hs
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
```

### 7.
Don’t ask for more type class instances than you need. You can let GHC tell you what to do:

```hs
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
```

---

```hs
instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)
```

### 8. 
```hs
data Notorious g o a t = Notorious (g o) (g a) (g t)
```

---

```hs
instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
```

Why doesn't this compile?
```hs
instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) 
    = Notorious (fmap f go) (fmap f ga) (fmap f gt)
```

### 9.
You’ll need to use recursion:
```hs
data List a 
  = Nil
  | Cons a (List a)
```

---

```hs
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)
```

### 10. 
A tree of goats forms the Goat-Lord, a fearsome poly-creature:

```hs
data GoatLord a 
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
```

---

```hs
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (fmap f gl1) (fmap f gl2) (fmap f gl3)
```

### 11.
You’ll use an extra functor for this one, although your solution might do it monomorphically without using fmap. Keep in mind that you will probably not be able to validate this one in the usual manner. Do your best to make it work:

```hs
data TalkToMe a 
  = Halt
  | Print String a
  | Read (String -> a)
```

---

```hs
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read s2a) = Read (f . s2a)
```

