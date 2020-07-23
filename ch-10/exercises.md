# Exercises

## Exercises: Understanding folds (pg.364)

### 1. 

```foldr (*) 1 [1..5]```

Will return the same result as which of the following?
- a) ```flip (*) 1 [1..5]```
- b) ```foldl (flip (*)) 1 [1..5]```
- c)

---

a) will crash, because is trying to multiply 1 with [1,2,3,4,5]
b) will return the same:

```hs
foldr (*) 1 [1..5]
-- 1 * (2 * (3 * (4 * (5 * 1))))
-- 120
```

```hs
foldl (flip (*)) 1 [1..5]
-- 5 * (4 * (3 * (2 * (1 * 1))))
-- 120
```

### 2. 
Write out the evaluation steps for:

```foldl (flip (*)) 1 [1..3]```

---

```hs
-- (3 * (2 * (1 * 1)))
-- 6
```

### 3. 
One difference between `foldr` and `foldl` is:

- a) `foldr`, but not `foldl`, traverses the spine of a list from right to left.
- b) `foldr`, but not `foldl`, always forces the rest of the fold.
- c) `foldr`, but not `foldl`, associates to the right.
- d) `foldr`, but not `foldl`, is recursive.

---

- a) False, because both traverse from left to right
- b) False, because foldr can skip the rest of the fold when using `const`, `any` or the like.
- c) True
- d False, both are recursive

### 4. 
Folds are catamorphisms, which means they are generally used to:

- a) Reduce structure. 
- b) Expand structure.
- c) Render you catatonic.
- d) Generate infinite data structures.

---

- a) Reduce structure. 

### 5. 
The following are simple folds very similar to what you’ve already seen, but each has at least one error. Please fix and test them in your REPL:
- a) `foldr (++) ["woot", "WOOT", "woot"]`
- b) `foldr max [] "fear is the little death"`
- c) `foldr and True [False, True]`
- d) This one is more subtle than the previous. Can it ever return a different answer? `foldr (||) True [False, True]`
- e) `foldl ((++) . show) "" [1..5]`
- f) `foldr const 'a' [1..5]`
- g) `foldr const 0 "tacos"`
- h) `foldl (flip const) 0 "burritos"`
- i) `foldl (flip const) 'z' [1..5]`

---

- a) 

```hs
foldr (++) "" ["woot", "WOOT", "woot"]
```

Added initial neutral value for strings: `""`.

- b) 

```
import Data.Char
foldr max (minBound :: Char) "fear is the little death"
```

Corrected initial neutral value for `Char`, which is `chr 0`.

- c)

`foldr (&&) True [False, True]`

Changed `and` for `&&`, because `and` type signature does not fit here, because expects `[Bool]`.

- d)

`foldr (||) False [False, True]`

The initial neutral value should be `False`, because `True` would always return `True`.

- e)

`foldl (\s x -> show x ++ s) "" [1..5]`

That function composition wasn't working...

- f)

The issue with `foldr const 'a' [1..5]` is that it expects the return value to ba a `Char`, but it returns `1`, a Num.

We could fix it like this:

`foldr const 0 [1..5]`

- g)

Same as before, but now it expects a `Num` as return type, but it returns a `Char`.

We could fix it like this:

`foldr const 'a' "tacos"`

- h)

Same, return types don't match:

`foldl (flip const) 'a' "burritos"`

- i)

Same, return types don't match:

`foldl (flip const) 0 [1..5]`



## Exercises: Database processing (pg.370)

Let’s write some functions to process the following data:

```hs
import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime 
                  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem] 
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123)) 
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
  ]
```

### 1. 
Write a function that filters for `DbDate` values and returns a list of the `UTCTime` values inside them:
```hs
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = undefined
```

---

```hs
utcTime :: DatabaseItem -> [UTCTime]
utcTime item 
  = case item of
    DbString _ -> []
    DbNumber _ -> []
    DbDate t -> [t]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = foldr (\a b -> (utcTime a) ++ b) [] db
```

### 2. 
Write a function that filters for `DbNumber` values and returns a list of the `Integer` values inside them:

```hs
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = undefined
```

---

```hs
integer :: DatabaseItem -> [Integer]
integer item 
  = case item of
    DbNumber n -> [n]
    _ -> []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr (\a b -> (integer a) ++ b) [] db
```

### 3. 
Write a function that gets the most recent date:

```hs
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = undefined
```

---

```hs
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db 
  = head 
  . reverse 
  . sort 
  . filterDbDate 
  $ db
```

### 4. 
Write a function that sums all of the `DbNumber` values:

```hs
sumDb :: [DatabaseItem] -> Integer
sumDb = undefined
```

---

```hs
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber
```

### 5. 
Write a function that gets the average of the `DbNumber` values:

```hs
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb = undefined
```

---

```hs
avg :: [Integer] -> Double
avg xs = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)

avgDb :: [DatabaseItem] -> Double
avgDb db = avg . filterDbNumber $ db
```



## Missing exercises

## 378
```hs
--1.
fibs = 1 : scanl (+) 1 fibs
fibs20 = take 20 fibs 
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]


--2.
fibs100 = takeWhile (<100) fibs 
-- fibs100  ->  [1,1,2,3,5,8,13,21,34,55,89]

--3.
factorialScan n = (scanl (*) 1 [1..n]) !! n
-- factorialScan 10  ->  3628800


{-

scanl (+) 1 [1..3]
[1, 1 + 1, (1 + 1) + 2, ((1 + 1) + 2) + 3]
[1,2,4,7]


fibs :: Num a => [a]
fibs = 1 : scanl (+) 1 fibs

fibs = 1 : scanl (+) 1 []
       1 : [1]          
       [1,1]

fibs = 1 : scanl (+) 1 [1,1]
       1 : [1,1+1,(1+1)+1] 
       [1,1,2,3]

fibs = 1 : scanl (+) 1 [1,1,2,3]
       1 : [1,1+1,(1+1)+1,((1+1)+1)+2,(((1+1)+1)+2)+3)] 
       [1,1,2,3,5,8]

fibs' = 1 : scanl (+) 0 fibs'
      = [1,0,1,1,2,3,5...

-}
```

## 10.10 Warm Up andd review
```hs

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

{- 
1.a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.
This list will contain duplicates.
-}
stopVowelStop :: [String]
stopVowelStop = [a : (b : [c]) | a <- stops, b <- vowels, c <- stops]

stopVowelStops' :: [(Char, Char, Char)]
stopVowelStops' = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]


{-
1.b) Modify that function so that it only returns the combinations that begin with a p.
-}
stopVowelStops'' :: [(Char, Char, Char)]
stopVowelStops'' = [(s1, v, s2) | s1 <- stops, s1 == 'p', v <- vowels, s2 <- stops]
-- Uli
sVsWithP = [('p', b, c)]) | b <- vowels, c <- stops]

--Amin
--b
stopVowelStops''' = [(s1,v,s2) | s1 <- take 1 stops, v <- vowels, s2 <- stops ]
```


## pg.379 Rewrite functions as folds

```hs
-- Rewriting functions using folds
-- 1. myOr returns True if any Bool in the list is True:
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny returns True if a -> Bool applied to any of the values in the
-- list returns True:
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False
-- Uli
myAny f = myOr . map f
--Amin
myAny f = foldr (\x b -> f x || b) False

-- 3. Write two versions of myElem. One version should use folding and the
-- other should use any:
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> b || a == x) False

-- Uli
myElem e = foldr ((||) . (== e)) False

-- 4. Implement myReverse. Don’t worry about trying to make it lazy:
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
-- Amin
myReverse = foldr (\a b -> b ++ [a] ) []

-- 5. Write myMap in terms of foldr. It should have the same behavior as the
-- built-in map:
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6. Write myFilter in terms of foldr. It should have the same behavior as
-- the built-in filter:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr filtr []
  where filtr el acc = bool acc (el:acc) (f el)

--Amin
myFilter f = foldr (\a b -> bool [] [a] (f a) ++ b) []
myFilter' f = foldr (\a -> bool id (a:) (f a)) []

-- 7. squish flattens a list of lists into a list:
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap maps a function over a list and concatenates the result:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

--Amin
squishMap :: (a->[b]) -> [a] -> [b]
squishMap f = foldr (\a b -> foldr (:) b (f a)) []

-- 9. squishAgain flattens a list of lists into a list. This time, re-use the
-- squishMap function:
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy takes a comparison function and a list and returns the
-- greatest element of the list based on the last value that the comparison
-- returns GT for:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> bool b a (f a b == GT)) (last xs) xs
-- Uli
myMaximumBy cmp xs = foldl (\a b -> bool b a (GT == cmp a b)) (head xs) xs
myMaximumBy cmp l@(x:xs) = foldl (\a b -> bool b a (GT == cmp a b)) x l

-- Andrew
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "failing like Prelude"
myMaximumBy f (a:as) = foldr mmax a (reverse as)
  where mmax el max = bool el max (f max el == GT)


-- 11. myMinimumBy takes a comparison function and a list and returns the least 
-- element of the list based on the last value that the comparison returns LT 
-- for:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp l@(x:xs) = foldl (\a b -> bool b a (LT == cmp a b)) x l
```
