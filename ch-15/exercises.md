## Exercise: Optional Monoid (pg.589)

Write the `Monoid` instance for our `Maybe` type, renamed to `Optional`:

```hs
data Optional a 
  = Nada
  | Only a
  deriving (Eq, Show)
instance Monoid a => Monoid (Optional a) where
  mempty = undefined
  mappend = undefined
```

Expected output:

```
Prelude> onlySum = Only (Sum 1)
Prelude> onlySum `mappend` onlySum
Only (Sum {getSum = 2})

Prelude> onlyFour = Only (Product 4)
Prelude> onlyTwo = Only (Product 2)
Prelude> onlyFour `mappend` onlyTwo
Only (Product {getProduct = 8})

Prelude> Only (Sum 1) `mappend` Nada
Only (Sum {getSum = 1})

Prelude> Only [1] `mappend` Nada
Only [1]

Prelude> Nada `mappend` Only (Sum 1)
Only (Sum {getSum = 1})
```

---

```hs
import Data.Monoid

data Optional a 
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only $ x <> y
  (<>) (Only x) Nada = Only x
  (<>) Nada (Only y) = Only y
  (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)
```



## 15.11 Madness (pg.596)

```hs
import Data.Monoid

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <> 
  noun <> " and drove off with his " <> 
  adj <> " wife."
```

Rewrite it using `mconcat`:

```hs
madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = undefined
```

---

```hs
madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj 
  = mconcat 
  [ e , "! he said "
  , adv, " as he jumped into his car "
  , noun, " and drove off with his "
  , adj, " wife." 
  ]
```




## Exercise: Maybe another Monoid (pg.603)
Write a `Monoid` instance for a `Maybe` type that doesn’t require a `Monoid` for the contents. Reuse the `Monoid` law QuickCheck properties, and use them to validate the instance.

---

```hs
instance Semigroup (First' a) where 
  (<>) (First' (Only x)) _ = (First' (Only x))
  (<>) (First' Nada) (First' (Only x)) = (First' (Only x))
  (<>) _ _ = First' Nada

instance Monoid (First' a) where 
  mempty = First' Nada

--

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do 
  x <- arbitrary
  frequency 
    [ (1, return $ First' Nada) 
    , (1, return $ First' (Only x)) 
    ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen
```



## 15.15 Chapter exercises (pg.609)
## Semigroup exercises

Given a datatype, implement the `Semigroup` instance. Add `Semigroup` constraints to type variables where needed. Use the `Semigroup` class from base or write your own. When we use `<>`, we mean the infix `mappend` operation from the `Semigroup` type class.

### 1.

```hs
data Trivial 
  = Trivial 
  deriving (Eq, Show) 

instance Semigroup Trivial where
  _ <> _ = undefined

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO () 
main = quickCheck (semigroupAssoc :: TrivAssoc)
````

---

```hs
instance Semigroup Trivial where
  _ <> _ = Trivial
```

### 2.
```hs
newtype Identity a = Identity a
```

---

```hs
newtype Identity a 
  = Identity a 
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do 
  x <- arbitrary
  return $ Identity x
  
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = genIdentity

type IdentityAssoc = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

mainIdentity :: IO () 
mainIdentity = quickCheck (semigroupAssoc :: IdentityAssoc)
```

### 3.
```hs
data Two a b = Two a b
```

---

```hs
data Two a b 
  = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type MyTwo = Two (Product Int) (Sum Int)
type TwoAssoc = MyTwo -> MyTwo -> MyTwo -> Bool

mainTwo :: IO () 
mainTwo = quickCheck (semigroupAssoc :: TwoAssoc)

```

### 4.
```hs
data Three a b c = Three a b c
```

---

```hs
data Three a b c 
  = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type MyThree = Three (Sum Int) (Sum Int) (Product Int)
type ThreeAssoc = MyThree -> MyThree -> MyThree -> Bool

mainThree :: IO ()
mainThree = quickCheck (semigroupAssoc :: ThreeAssoc)
```

### 5.
```hs
data Four a b c d = Four a b c d
```

---

```hs
data Four a b c d 
  = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' = Four (a <> a')  (b <> b')  (c <> c')  (d <> d')

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

type MyFour = Four (Sum Int) (Sum Int) (Product Int) (Product Int)
type FourAssoc = MyFour -> MyFour -> MyFour -> Bool

mainFour :: IO ()
mainFour = quickCheck (semigroupAssoc :: FourAssoc)
```

### 6.
```hs
newtype BoolConj = BoolConj Bool
```

What it should do:
```
Prelude> (BoolConj True) <> (BoolConj True)
BoolConj True
Prelude> (BoolConj True) <> (BoolConj False)
BoolConj False
```

---

```hs
newtype BoolConj 
  = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

boolConjGen :: Gen BoolConj
boolConjGen 
  = elements 
  [ BoolConj True
  , BoolConj False
  ]

instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

mainBoolConj :: IO ()
mainBoolConj = quickCheck (semigroupAssoc :: BoolConjAssoc)
```

### 7.
```hs
newtype BoolDisj = BoolDisj Bool
```

What it should do:

```
Prelude> (BoolDisj True) <> (BoolDisj True)
BoolDisj True
Prelude> (BoolDisj True) <> (BoolDisj False)
BoolDisj True
```

---

```hs
newtype BoolDisj 
  = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

boolDisjGen :: Gen BoolDisj
boolDisjGen 
  = elements 
  [ BoolDisj True
  , BoolDisj False
  ]

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
mainBoolDisj :: IO ()
mainBoolDisj = quickCheck (semigroupAssoc :: BoolDisjAssoc)
```

### 8.
```hs
data Or a b 
  = Fst a
  | Snd b
```

The Semigroup for `Or` should have the following behavior. We can think of it as having a “sticky” `Snd` value, whereby it’ll hold onto the first `Snd` value when and if one is passed as an argument.

```
Prelude> Fst 1 <> Snd 2
Snd 2
Prelude> Fst 1 <> Fst 2
Fst 2
Prelude> Snd 1 <> Fst 2
Snd 1
Prelude> Snd 1 <> Snd 2
Snd 1
```

---

```hs
data Or a b 
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _ = Snd x
  Fst _ <> Snd x = Snd x
  Fst x <> Fst y = Fst y

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof  [ return $ Fst a
         , return $ Snd b
         ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type OrType = Or Int Char
type OrAssoc = OrType -> OrType -> OrType -> Bool
mainOr :: IO ()
mainOr = quickCheck (semigroupAssoc :: OrAssoc)
```

### 9.

newtype Combine a b 
  = Combine 
  { unCombine :: (a -> b) }

What it should do:
```
Prelude> f = Combine $ \n -> Sum (n + 1)
Prelude> g = Combine $ \n -> Sum (n - 1)

Prelude> unCombine (f <> g) $ 0
Sum {getSum = 0}

Prelude> unCombine (f <> g) $ 1
Sum {getSum = 2}

Prelude> unCombine (f <> f) $ 1
Sum {getSum = 4}

Prelude> unCombine (g <> f) $ 1
Sum {getSum = 2}
```

---

```hs
newtype Combine a b 
  = Combine 
  { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine a2b <> Combine a2b' = Combine $ a2b <> a2b'

-- Unable to do the quickCheck part

-- Andrew's tests
    it "9.e" $ property $ \(Fn f) (Fn g) x ->
      let cf  = Combine (f :: Integer -> String)
          cg  = Combine (g :: Integer -> String)
          exp = f x <> g x
          act = unCombine (cf <> cg) x
      in  act `shouldBe` exp
```

### 10.

```hs
newtype Comp a 
  = Comp { unComp :: (a -> a) }
```

---

```hs
newtype Comp a 
  = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)
```

### 11.
```hs
data Validation a b 
  = Failure a 
  | Success b 
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) = undefined
```

---

```hs
instance Semigroup a => Semigroup (Validation a b) where
  Succez s <> _          = Succez s
  Failurez _ <> Succez s  = Succez s
  Failurez f <> Failurez f' = Failurez $ f <> f'
```



## Monoid exercises (pg.612)

Given a datatype, implement the `Monoid` instance. Add `Monoid` constraints to type variables where needed. For the datatypes for which you’ve already implemented `Semigroup` instances, you need to figure out what the identity value is.

### 1.

```hs
data Trivial 
  = Trivial 
  deriving (Eq, Show)

instance Semigroup Trivial where 
  _ (<>) _ = Trivial

instance Monoid Trivial where 
  mempty = Trivial
  mappend = (<>)
```

### 2.

```hs
newtype Identity a 
  = Identity a 
  deriving Show
```

---

```hs
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

mainIdentityMonoid :: IO () 
mainIdentityMonoid = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity 
      mlr = monoidRightIdentity
  quickCheck (sa :: IdentityAssoc) 
  quickCheck (mli :: (Identity (Sum Int)) -> Bool) 
  quickCheck (mlr :: (Identity (Sum Int)) -> Bool)
```

### 3.

```hs
data Two a b 
  = Two a b 
  deriving Show
```

---

```hs
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

mainTwoMonoid :: IO () 
mainTwoMonoid = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity 
      mlr = monoidRightIdentity
  quickCheck (sa :: TwoAssoc) 
  quickCheck (mli :: (Two (Sum Int) (Product Int)) -> Bool) 
  quickCheck (mlr :: (Two (Sum Int) (Product Int)) -> Bool)
```

### 4.

```hs
newtype BoolConj 
  = BoolConj Bool
```

What it should do:
```
Prelude> (BoolConj True) `mappend` mempty
BoolConj True
Prelude> mempty `mappend` (BoolConj False)
BoolConj False
```

---

```hs
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)
```

### 5.

```hs
newtype BoolDisj 
  = BoolDisj Bool
```

What it should do:

```
Prelude> (BoolDisj True) `mappend` mempty
BoolDisj True
Prelude> mempty `mappend` (BoolDisj False)
BoolDisj False
```

---

```hs
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)
```

### 6.

```hs
newtype Combine a b =
  Combine { unCombine :: (a -> b) }
```

What it should do:
```
Prelude> f = Combine $ \n -> Sum (n + 1)
Prelude> unCombine (mappend f mempty) $ 1
Sum {getSum = 2}
```

---

```hs
instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)
```

### 7.

```hs
newtype Comp a = Comp (a -> a)
```

---

```hs
instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

{- Example usage:
λ> f = Comp $ \sum -> sum <> (Sum 1)
λ> unComp (mappend f mempty) $ Sum 1
Sum {getSum = 2}
-}
```

### 8

 ```hs
newtype Mem s a = 
  Mem {
    runMem :: s -> (a,s) 
  }

instance Semigroup a => Semigroup (Mem s a) where 
  (<>) = undefined

instance Monoid a => Monoid (Mem s a) where 
  mempty = undefined
```

Given the following code:

```hs
f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int)) 
  print $ rmleft == runMem f' 0 
  print $ rmright == runMem f' 0
```

A correct Monoid for `Mem` should, given the above code, produce the following output:

```
Prelude> main
("hi",1)
("hi",1)
("",0)
True True
```

Make certain your instance has output like the above, as this is sanity checking the Monoid identity laws for you! It’s not a proof, and it’s not even as good as property testing, but it’ll catch the most common mistakes people make.
It’s not a trick, and you don’t need a Monoid for s. Yes, such a Monoid can and does exist. Hint: chain the s values from one function to the other. You’ll want to check the identity laws, as a common first attempt will break them.

---

```hs
instance Semigroup a => Semigroup (Mem s a) where 
  Mem s2as <> Mem s2as' = Mem 
    $ (\(a, s) -> 
        let 
          (a', s') = s2as s
        in
          (a <> a', s')
      ) 
    . s2as' -- The s2as function only calculates `s` from `(a, s)`, the concatenation of `a` has 
            -- to be done manually outside the funciton, like: `(a <> a', s)`.

instance Monoid a => Monoid (Mem s a) where 
  mempty = Mem (\x -> (mempty, x)) -- here `mempty` takes its value from the provided `a` type. 
                                   -- In case of `String` it woulb be `""` (String is a monoid!), in 
                                   -- case of `Sum Int`, it would be `Sum 0`.

```
