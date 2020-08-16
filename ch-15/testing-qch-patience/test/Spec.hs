import Control.Monad
import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a

---------------------------------
-- Bull
---------------------------------

data Bull 
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where 
  arbitrary 
    = frequency 
    [ (1, return Fools) 
    , (1, return Twoo) 
    ]

instance Semigroup Bull where 
  (<>) _ _ = Fools

instance Monoid Bull where 
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

mainBull :: IO () 
mainBull = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity 
      mri = monoidRightIdentity
  quickCheck (ma :: BullMappend) 
  quickCheck (mli :: Bull -> Bool) 
  quickCheck (mri :: Bull -> Bool)


---------------------------------
-- Optional a <- `a` is not a Monoid
---------------------------------

data Optional a 
  = Nada
  | Only a
  deriving (Eq, Show)

newtype First' a 
  = First' 
  { getFirst' :: Optional a 
  } deriving (Eq, Show)

instance Semigroup (First' a) where 
  (<>) (First' (Only x)) _ = (First' (Only x))
  (<>) (First' Nada) (First' (Only x)) = (First' (Only x))
  (<>) _ _ = First' Nada

instance Monoid (First' a) where 
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend 
  = First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

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

--
mainFirst :: IO () 
mainFirst = do
  quickCheck (monoidAssoc :: FirstMappend) 
  quickCheck (monoidLeftIdentity :: FstId) 
  quickCheck (monoidRightIdentity :: FstId)


---------------------------------
-- Chapter exercises
-- Semigroup
---------------------------------

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1

data Trivial 
  = Trivial 
  deriving (Eq, Show) 

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

mainTrivial :: IO () 
mainTrivial = quickCheck (semigroupAssoc :: TrivAssoc)

-- 2.

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

-- 3.

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

-- 4.

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

-- 5.

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

-- 6.

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

-- 7.

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

-- 8.

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

-- 9.

newtype Combine a b 
  = Combine 
  { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine a2b <> Combine a2b' = Combine $ a2b <> a2b'

{-
f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)
unCombine (f <> g) $ 0
-}

-- combineGen :: Gen (Combine a b)
-- combineGen = do
--   a2b <- (Fn (f :: a -> b))
--   return $ Combine a2b

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = combineGen

-- type CombineType = Combine Int (Sum Int)
-- type CombineAssoc = CombineType -> CombineType -> CombineType -> Bool
-- mainCombine :: IO ()
-- mainCombine = quickCheck (semigroupAssoc :: CombineAssoc)

-- 10.

newtype Comp a 
  = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp f' = Comp $ f <> f'

{- Example usage:
λ> f = Comp $ \s -> s <> (Sum 1)
λ> g = Comp $ \s -> s <> (Sum (-1))
λ> unComp (f <> g)
Sum {getSum = 0}
-}

-- 11.

data Validation a b 
  = Failurez a 
  | Succez b 
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Succez s <> _          = Succez s
  Failurez _ <> Succez s  = Succez s
  Failurez f <> Failurez f' = Failurez $ f <> f'

failurez :: String -> Validation String Int
failurez = Failurez

succez :: Int -> Validation String Int
succez = Succez

mainValidation = do
  print $ succez 1 <> failurez "blah" 
  print $ failurez "woot" <> failurez "blah" 
  print $ succez 1 <> succez 2
  print $ failurez "woot" <> succez 2



---------------------------------
-- Chapter exercises
-- Monoid
---------------------------------

-- 1.

-- data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where 
  mempty = Trivial
  mappend = (<>)

mainTrivialMonoid :: IO () 
mainTrivialMonoid = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity 
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc) 
  quickCheck (mli :: Trivial -> Bool) 
  quickCheck (mlr :: Trivial -> Bool)

-- 2.

-- newtype Identity a = Identity a deriving Show

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

-- 3.

-- data Two a b = Two a b deriving Show

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

-- 4.

-- newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

mainBoolConjMonoid :: IO () 
mainBoolConjMonoid = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity 
      mlr = monoidRightIdentity
  quickCheck (sa :: BoolConjAssoc) 
  quickCheck (mli :: BoolConj -> Bool) 
  quickCheck (mlr :: BoolConj -> Bool)

-- 5.

-- newtype BoolDisj = BoolDisj Bool

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

mainBoolDisjMonoid :: IO () 
mainBoolDisjMonoid = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity 
      mlr = monoidRightIdentity
  quickCheck (sa :: BoolDisjAssoc) 
  quickCheck (mli :: BoolDisj -> Bool) 
  quickCheck (mlr :: BoolDisj -> Bool)


-- 6.

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

{-
Prelude> f = Combine $ \n -> Sum (n + 1)
Prelude> unCombine (mappend f mempty) $ 1
Sum {getSum = 2}
-}

-- 7.

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

{- Example usage:
λ> f = Comp $ \sum -> sum <> (Sum 1)
λ> unComp (mappend f mempty) $ Sum 1
Sum {getSum = 2}
-}

-- 8.

newtype Mem s a = 
  Mem {
    runMem :: s -> (a, s) 
  }

instance Semigroup a => Semigroup (Mem s a) where 
  Mem s2as <> Mem s2as' = Mem 
    $ (\(a, s) -> 
        let 
          (a', s') = s2as s
        in
          (a <> a', s')
      ) 
    . s2as' -- The s2as function only calculates `s` from `(a, s)`, the concatenation of `a` has to be done manually outside the funciton, like: `(a <> a', s)`.

instance Monoid a => Monoid (Mem s a) where 
  mempty = Mem (\x -> (mempty, x)) -- here `mempty` takes its value from the provided `a` type. 
                                   -- In case of `String` it woulb be `""` (String is a monoid!), in case of `Sum Int`, it would be `Sum 0`.

f' = Mem $ \s -> ("hi", s + 1)

mainMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int)) 
  print $ rmleft == runMem f' 0 
  print $ rmright == runMem f' 0

{-
Prelude> mainMem
("hi",1)
("hi",1)
("",0)
True 
True
-}