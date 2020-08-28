import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid

-- 1.

added :: Maybe Integer 
added =
  (+3) <$> (lookup (3::Integer) $ zip [1, 2, 3] [4, 5, 6])

-- 2.

y2 :: Maybe Integer
y2 = lookup (3::Integer) $ zip [1, 2, 3] [4, 5, 6] 

z2 :: Maybe Integer
z2 = lookup (2::Integer) $ zip [1, 2, 3] [4, 5, 6] 

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y2 <*> z2

-- 3.

x3 :: Maybe Int
x3 = elemIndex (3::Int) [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex (4::Int) [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int 
max' = max

{-
max <$> Just 2 <*> Just 3
Just (max 2 3)
-}

maxed :: Maybe Int 
maxed = max' <$> x3 <*> y3

-- 4.

xs :: [Integer]
xs = [1, 2, 3] 

ys :: [Integer]
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer 
summed = fmap sum $ (,) <$> x4 <*> y4


-- ## Exercise: Identity instance (pg.692)

newtype Identity a 
  = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where 
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

{-
λ> const <$> Identity [1,2,3] <*> Identity [9,9,9]
Identity [1,2,3]
λ> (Identity $ const [1,2,3]) <*> Identity [9,9,9]
Identity [1,2,3]
-}


-- ## Exercise: Constant instance (pg.693)

newtype Constant a b 
  = Constant { getConstant :: a } 
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure = Constant . mempty
  Constant x <*> Constant y = Constant $ mappend x y

{-
λ> f = Constant (Sum 1)
λ> g = Constant (Sum 2)
λ> f <*> g
Constant {getConstant = Sum {getSum = 3}}
-}



-- ## Exercise: Fixer upper (pg.707)

-- 1.
ex707_1 :: Maybe String
ex707_1 = const <$> Just "Hello" <*> pure "World"

-- 2.
ex707_2 :: Maybe (Integer, Integer, [Char], [Integer])
ex707_2 
  = (,,,) 
  <$> Just 90 
  <*> Just 10 
  <*> Just "Tierness" 
  <*> pure [1, 2, 3]


-- Combos

stops :: String
stops = "pbtdkg" 

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\a b c -> (a, b, c))

