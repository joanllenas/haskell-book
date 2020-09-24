## Exercises: Library functions (pg.818)

Implement the functions in terms of `foldMap` or `foldr` from `Foldable`, then try them out with multiple types that have `Foldable` instances.

### 1. 
```hs
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

{-
λ> sum' $ [Product 4, Product 5]
Product {getProduct = 9}
λ> sum' $ Sum 4
4
-}
```

### 2.
```hs
product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

{-
λ> product' $ [Product 4, Product 5]
Product {getProduct = 20}
λ> product' $ Sum 4
4
-}
```

### 3.
```hs
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\a b -> x == a || b) False

{-
λ> elem' 1 $ [Sum 1, Sum 2]
True
λ> elem' 1 $ [3, 2]
False
λ> elem' 1 $ Just 1
True
λ> elem' 1 $ Nothing
False
-}
```

### 4.
```hs
getMin :: (Ord a) => a -> Maybe a -> Maybe a
getMin a ma@(Just _) = fmap (\x -> bool x a (a < x)) ma
getMin a Nothing = Just a

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' ta = foldr (\a b -> getMin a b) Nothing ta

{-
λ> minimum' []
Nothing
λ> minimum' Nothing
Nothing
λ> minimum' [2,1,3,4]
Just 1
λ> minimum' [Sum 2, Sum 1, Sum 3]
Just (Sum {getSum = 1})
-}

{-
Andrew:
import Data.Semigroup

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = getMin <$> foldr ((<>) . Just . Min) Nothing xs
-}
```

### 5. 
```hs
getMax :: (Ord a) => a -> Maybe a -> Maybe a
getMax a ma@(Just _) = fmap (\x -> bool a x (a < x)) ma
getMax a Nothing = Just a

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\a b -> getMax a b) Nothing

{-
λ> maximum' []
Nothing
λ> maximum' Nothing
Nothing
λ> maximum' [2,1,3,4]
Just 4
λ> maximum' [Sum 2, Sum 1, Sum 3]
Just (Sum {getSum = 3})
-}
```

### 6.

```hs
nil :: (Foldable t) => t a -> Bool
nil = foldr (\a b -> False) True

{--
λ> nil []
True
λ> nil [2]
False
λ> nil Nothing
True
λ> nil $ Just 3
False
-}
```

### 7.
```hs
len :: (Foldable t) => t a -> Int
len = foldr (\_ b -> b + 1) 0

{-
λ> len []
0
λ> len [1]
1
λ> len Nothing
0
λ> len $ Just 5
-}

{-
length'' :: (Foldable t) => t a -> Int
length'' = getSum . foldMap (const $ Sum 1)
-}
```

### 8.

```hs
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\a b -> a : b) []

{-
λ> toList' Nothing
[]
λ> toList' $ [1,2,3]
[1,2,3]
λ> toList' $ Just [1,2,3]
[[1,2,3]]
λ> toList' $ Just 3
[3]
λ> toList' []
[]
-}
```

### 9.
Combine the elements of a structure using a `Monoid`.

```hs
fold' :: (Foldable t, Monoid m) => t m -> m
-- fold' = foldMap (mempty <>)

-- Andrew
fold' = foldMap id

{-
λ> fold' $ Just (Sum 2)
Sum {getSum = 2}
λ> fold' $ Nothing::Sum Int
Sum {getSum = 0}
λ> fold' $ [Sum 2, Sum 3]
Sum {getSum = 5}
λ> fold' $ [Product 2, Product 3]
Product {getProduct = 6}
-}
```

### 10.
Define foldMap in terms of foldr:
```hs
foldAndMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldAndMap a2b = foldr (\a b -> a2b a <> b) mempty

{-
λ> foldAndMap Sum [1, 2, 3, 4]
Sum {getSum = 10}
λ> foldAndMap Sum []
Sum {getSum = 0}
λ> foldAndMap Product [3, 4]
Product {getProduct = 12}
λ> foldAndMap Product $ Just 3
Product {getProduct = 3}
λ> foldAndMap Product Nothing
Product {getProduct = 1}
-}
```

## 20.6 Chapter exercises (pg. 819)

Write Foldable instances for the following datatypes: 

### 1.
```hs
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f i (Constant x) = f x i
```

### 2.
```hs
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f i (Two x y) = f y i
```

### 3.
```hs
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f i (Three x y z) = f z i
```

### 4.
```hs
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap a2m (Three' a b c) = a2m b <> a2m c

-- Also valid? foldMap a2m (Three' a b c) = a2m b
-- Also valid? foldMap a2m (Three' a b c) = a2m c
```

### 5.
```hs
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap a2m (Four' a b c d) = a2m b <> a2m c <> a2m d

-- Also valid? foldMap a2m (Four' a b c d) = a2m b
-- Also valid? foldMap a2m (Four' a b c d) = a2m c
-- Also valid? foldMap a2m (Four' a b c d) = a2m d
```

### 6.
Thinking cap time. Write a `filter` function for `Foldable` types using the `foldMap` function:

```hs
filterF 
  :: ( Applicative f , Foldable t , Monoid (f a)) 
  => (a -> Bool) -> t a -> f a
filterF = undefined
```
