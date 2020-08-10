module Optional where

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

--

onlySum :: Optional (Sum Int)
onlySum = Only (Sum 1)
res1 :: Bool
res1 = onlySum `mappend` onlySum == Only (Sum 2)

onlyFour :: Optional (Product Int)
onlyFour = Only (Product 4)
onlyTwo :: Optional (Product Int)
onlyTwo = Only (Product 2)
res2 :: Bool
res2 = onlyFour `mappend` onlyTwo == Only (Product 8)

res3 :: Bool
res3 = Only (Sum 1) `mappend` Nada == Only (Sum (1::Int))

res4 :: Bool
res4 = Only [1::Int] `mappend` Nada == Only [1]

res5 :: Bool
res5 = Nada `mappend` Only (Sum (1::Int)) == Only (Sum 1)

res :: Bool
res = res1 && res2 && res3 && res4 && res5
