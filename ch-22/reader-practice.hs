module ReaderPractice where

-- -------------------------------------------
-- 22.11 Chapter Exercises
-- A warm-up stretch (pg.867)
-- -------------------------------------------

import Control.Applicative
import Data.Maybe
import Data.Monoid

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y -- Just 6

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z -- Just 9

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y -- Nothing

-- now zip x and z using a, variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z -- z' 2 = Just 8; z' 88 = Nothing

-- Maybe (,)

--- Tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys -- Just (6,9)

--- Tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs -- Nothing

--- Takes one input and makes a tuple of the results of two applications of z' from above.
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n) -- x3 3 = (Just 9, Just 9)

--

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt n = n > 3 && n < 8

main1 :: IO ()
main1 = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(> 3), (> 8), even] 7 -- We have a Reader for the Applicative (functions) and a traversable for the list

{-
λ> main1
Just [3,2,1]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
Just [6,9]
Just 15
Nothing
True
[True,False,False]
[True,False,False]
-}

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(> 3), (< 8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ getAll $ foldr (\a b -> b <> All a) mempty (sequA 3) -- 1. (pg.871)
  print $ sequA $ fromMaybe 3 s' -- 2.
  print $ bolt $ fromMaybe 3 ys -- 3.