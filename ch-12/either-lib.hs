module EitherLib where

import Data.Bool

-- recursive version
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' ((Left x):xs) = x : lefts' xs
lefts' ((Right _):xs) = lefts' xs

-- foldr version
leftAsList :: Either a b -> [a]
leftAsList (Left a) = [a]
leftAsList _ = []

leftsFoldr :: [Either a b] -> [a]
leftsFoldr =  foldr (\x y -> (leftAsList x) ++ y) []

--

-- recursive version
rights' :: [Either a b] -> [b]
rights' [] = []
rights' ((Left _):xs) = rights' xs
rights' ((Right x):xs) = x : rights' xs

-- foldr version
rightAsList :: Either a b -> [b]
rightAsList (Right x) = [x]
rightAsList _ = []

rightsFoldr :: [Either a b] -> [b]
rightsFoldr =  foldr (\x y -> (rightAsList x) ++ y) []

--

addIfLeft :: Either a b -> [a] -> [a]
addIfLeft (Left x) xs = x : xs
addIfLeft _ xs = xs

addIfRight :: Either a b -> [b] -> [b]
addIfRight (Right x) xs = x : xs
addIfRight _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' 
  = foldr 
  (\x (as, bs) -> (addIfLeft x as, addIfRight x bs)) 
  ([],[])

--

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' b2c (Right b) = Just $ b2c b
eitherMaybe' _ _ = Nothing

--

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' a2c _ (Left a) = a2c a
either' _ b2c (Right b) = b2c b

--

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' b2c x = either' (\_ -> Nothing) (Just . b2c) x

