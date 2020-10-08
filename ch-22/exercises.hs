{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Data.Char

-- ------------------------------------
-- Short Exercise: Warming up (pg.849)
-- ------------------------------------

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

{-
We want to return the results of `cap` and `rev` as a tuple.
Prelude> tupled "Julie"
("JULIE","eiluJ")
-- or
Prelude> tupled' "Julie"
("eiluJ","JULIE")
-}

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

{-
Do it one time using `do` syntax. Then try writing a new version using `>>=`.
-}

tupled_do :: [Char] -> ([Char], [Char]) -- I actually don't get this. How would this look like with explicit params? i.e. no point free style
tupled_do = do
  a <- cap
  b <- rev
  return (a, b)

tupled_bind :: [Char] -> ([Char], [Char])
--tupled_bind xs = return () >>= \_ -> (cap xs, rev xs) -- also works
tupled_bind xs = (cap xs, "") >>= \_ -> ("", rev xs)

-- ------------------------------------
-- Exercise Ask (pg.855)
-- ------------------------------------

newtype Reader r a = Reader {runReader :: r -> a}

--fmap :: Functor f => (a -> b) -> f a -> f b
instance Functor (Reader a) where
  fmap a2b (Reader fa) = Reader $ (a2b . fa)

ask :: Reader a a
ask = Reader id

-- -------------------------------------------
-- Exercise: Reading comprehension (pg.859)
-- -------------------------------------------

-- 1.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 a2b2c fa fb = a2b2c <$> fa <*> fb

-- 2.

asks :: (r -> a) -> Reader r a
asks r2a = Reader r2a

-- 3.

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  -- (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r) -- Simplified version
  (Reader rab) <*> (Reader ra) = Reader $ \r ->
    let a2bFn = (rab r)
        a = (ra r)
     in a2bFn a

-- -------------------------------------------
-- Exercise: Reader Monad (pg.864)
-- -------------------------------------------

-- 1.

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = join (Reader $ \r -> aRb (ra r))

-- 2.

-- Exercise boilerplate

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- The actual exercise

getDogReader :: Reader Person Dog
--getDogReader = Reader $ \p -> Dog (dogName p) (address p) -- Explicit version
getDogReader = Reader $ Dog <$> dogName <*> address

getDogR :: Person -> Dog
getDogR = runReader getDogReader
