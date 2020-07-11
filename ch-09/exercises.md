# Exercises

## Exercise: EnumFromTo (pg306)

Some things you’ll want to know about the `Enum` type class:

```
Prelude> :info Enum
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
Prelude> succ 0
1
Prelude> succ 1
2
Prelude> succ 'a'
'b'
```

Write your own `enumFromTo` definitions for the types provided. Do not use range syntax to do so. It should return the same results as if you did `[start..stop]`. Replace the `undefined`, a value that results in an error when evaluated, with your own definition.

```hs
eftBool :: Bool -> Bool -> [Bool] 
eftBool = undefined

eftOrd :: Ordering
      -> Ordering
      -> [Ordering]
eftOrd = undefined

eftInt :: Int -> Int -> [Int] 
eftInt = undefined

eftChar :: Char -> Char -> [Char] 
eftChar = undefined
```

---

```hs
eftBool :: Bool -> Bool -> [Bool] 
eftBool False False = [False]
eftBool True True = [True]
eftBool True False = [True, False]
eftBool False True = []


myFromTo :: (Eq a, Ord a, Enum a) => a -> a -> [a] -> [a]
myFromTo from to xs 
  | from > to = []
  | from == to = from : xs
  | otherwise = myFromTo (succ from) to (from : xs)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to = reverse $ myFromTo from to []


eftInt :: Int -> Int -> [Int] 
eftInt from to = reverse $ myFromTo from to []

eftChar :: Char -> Char -> [Char] 
eftChar from to = reverse $ myFromTo from to []
```



## Exercises: Thy fearful symmetry (pg.310)

### 1. 
Using `takeWhile` and `dropWhile`, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into words, as in the following sample:

```
Prelude> myWords "sheryl wants fun"
["sheryl", "wants", "fun"]
```

---

```hs
lTrim :: String -> String
lTrim [] = ""
lTrim (' ':xs) = lTrim xs
lTrim (x:xs) = x : xs

myWords :: String -> [String]
myWords "" = []
myWords s = 
  let
    str = takeWhile (\ch -> ch /= ' ') s
    rest = dropWhile (\ch -> ch /= ' ') s
  in
    [str] ++ myWords (lTrim rest)
```

### 2. 
Next, write a function that takes a string and returns a list of strings, using newline separators to break up the string, as in the following (your job is to fill in the undefined function):

```hs
module PoemLines where

firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
```

This is the result that putStrLn sentences should print:

```
Tyger Tyger, burning bright
In the forests of the night
What immortal hand or eye
Could frame thy fearful symmetry?
```

Implement this:
```
myLines :: String -> [String]
myLines = undefined
```

We want myLines sentences to equal:

```hs
shouldEqual =
[ "Tyger Tyger, burning bright"
, "In the forests of the night"
, "What immortal hand or eye"
, "Could frame thy fearful symmetry?" 
]
```

The main function here is a small test to ensure you’ve written your function correctly:


```hs
main :: IO () 
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
          == shouldEqual)
```

---

```hs
lTrimNewline :: String -> String
lTrimNewline [] = ""
lTrimNewline ('\n':xs) = lTrimNewline xs
lTrimNewline (x:xs) = x : xs

myLines :: String -> [String]
myLines "" = []
myLines s = 
  let
    str = takeWhile (\ch -> ch /= '\n') s
    rest = dropWhile (\ch -> ch /= '\n') s
  in
    [str] ++ myLines (lTrimNewline rest)
```

### 3. 
Now, let’s look at what those two functions have in common. Try writing a new function that parameterizes the character you’re breaking the string argument on and rewrite `myWords` and `myLines` using that parameter.

---

```hs
ltrimChar :: Char -> String -> String
ltrimChar _ [] = ""
ltrimChar ch (x:xs) = 
  if ch == x 
  then ltrimChar ch xs 
  else x : xs

splitWords :: Char -> String -> [String]
splitWords _ "" = []
splitWords char s = 
  let
    str = takeWhile (\ch -> ch /= char) s
    rest = dropWhile (\ch -> ch /= char) s
  in
    [str] ++ splitWords char (ltrimChar char rest)
```




## Exercises: Comprehend thy lists (pg.314)

Take a look at the following functions, determine what you think the output lists will be, and then run them in your REPL to verify (note that you will need the `mySqr` list from above in scope to do this):

```hs
mySqr = [x^2 | x <- [1..10]] -- [1,4,9,16,25,36,49,64,81,100]

[x | x <- mySqr, rem x 2 == 0] -- a)

[(x, y) | x <- mySqr
        , y <- mySqr
        , x < 50
        , y > 50
] -- b)

take 5 [(x, y) | x <- mySqr
               , y <- mySqr
               , x < 50
               , y > 50 
] -- c)
```

---

```hs
-- a)
[4,16,36,64,100]
-- b)
[
  (1,64),(1,81),(1,100),
  (4,64),(4,81),(4,100),
  (9,64),(9,81),(9,100),
  (16,64),(16,81),(16,100),
  (25,64),(25,81),(25,100),
  (36,64),(36,81),(36,100),
  (49,64),(49,81),(49,100)
]
-- c)
[(1,64),(1,81),(1,100),(4,64),(4,81)]
```



## Exercises: Square cube (pg.316)

Given the following:

```hs
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
````

### 1. 
First write an expression that will make tuples of the outputs of `mySqr` and `myCube`.

---

```hs
[(x,y) |
  x <- mySqr,
  y <- myCube
]
```

### 2. 
Now, alter that expression so that it only uses the `x` and `y` values that are less than `50`.

---

```hs
[(x,y) |
  x <- mySqr,
  y <- myCube,
  x < 50 && y < 50
]
```

### 3. 
Apply another function to that list comprehension to determine how many tuples inhabit your output list.

---

```hs
myTuple = [(x,y) |
  x <- mySqr,
  y <- myCube,
  x < 50 && y < 50
]
length myTuple
```




## Exercises: Bottom madness (pg.325)

Will it blow up?
Will the following expressions return a value or be `⊥`?

### 1.
```hs
[x^y | x <- [1..5], y <- [2, undefined]]
```

---

Bottom, because `x^y` reaches the `undefined` value.

### 2.
```hs
take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
```

---

It doesn't fail because we only evaluate the first item `1^2`.

### 3. 
```hs
sum [1, undefined, 3]
```

---

Blows up because `sum` reaches the `undefined` value.

### 4. 
```hs
length [1, 2, undefined]
```

---

It doesn't fail because items are not evaluated, only counted.

### 5. 
```hs
length $ [1, 2, 3] ++ undefined
```

---

Blows up becasue `[] ++ undefined` is evaluated.

### 6. 
```hs
take 1 $ filter even [1, 2, 3, undefined]
```

---

It doesn't fail because the first event number is found before reaching `undefined`.

### 7. 
```hs
take 1 $ filter even [1, 3, undefined]
```

---

It fails because the `undefined` is reached before finding the first even number.

### 8. 
```hs
take 1 $ filter odd [1, 3, undefined]
```

---

It doesn't fail because the first odd number is found before reaching `undefined`.

### 9. 
```hs
take 2 $ filter odd [1, 3, undefined]
```

---

It doesn't fail because the second odd number is found before reaching `undefined`.

### 10.
```hs
take 3 $ filter odd [1, 3, undefined]
```

---

It fails because the `undefined` value is reached before finding the third odd number.



## Intremission: Is it in normal form? (pg.326)

For each expression below, determine whether it’s in:

1. (NF) Normal Form, which implies Weak Head Normal Form.
2. (WHNF) Weak head normal form only. 
3. Neither.

Remember that an expression cannot be in NF or WHNF if the outermost part of the expression isn’t a data constructor. It can’t be in NF if any part of the expression is unevaluated:

### 1. 
```hs
[1, 2, 3, 4, 5]
```

---

NF

### 2. 
```hs
1 : 2 : 3 : 4 : _
```

---

WHNF because the `_` is not evaluated

### 3. 
```hs
enumFromTo 1 10
```

---

Is not in NF or WHNF

### 4. 
```hs
length [1, 2, 3, 4, 5]
```

---

Is not in NF or WHNF

### 5. 
```hs
sum (enumFromTo 1 10)
```

---

Is not in NF or WHNF

### 6. 
```hs
['a'..'m'] ++ ['n'..'z']
```

---

Is not in NF or WHNF

### 7. 
```hs
(_, 'b')
```

---

It's WHNF because `(,)` is a data constructor



## Exercises: More bottoms (pg.332)

As always, we encourage you to try figuring out the answers before you enter them into your REPL:

### 1. 
Will the following expression return a value or be `⊥`? 
```hs
take 1 $ map (+1) [undefined, 2, 3]
```

---

Bottom, because `take 1` forces the evaluation of `undefined + 1`.

### 2. 
Will the following expression return a value?
```hs
take 1 $ map (+1) [1, undefined, 3]
```

---

Returns `[2]` because `take 1` only forces evaluation of `1+1`.

### 3. 
Will the following expression return a value?

```hs
take 2 $ map (+1) [1, undefined, 3]
```

---

Bottom, because `take 2` forces evaluation of `2 + undefined`.

### 4. 
What does the following mystery function do? What is its type? Describe it (to yourself or a loved one) in standard English and then test it out in the REPL to make sure you are correct:
```hs
itIsMystery xs = map (\x -> elem x "aeiou") xs
```

---

```hs
itIsMystery :: String -> [Bool]
```

Given a `String`, iterates over all its characters and returns a list where each `String` character is converted to `True` if its a vowel or `False` otherwise.

### 5. 
What will be the result of the following functions:
- a) 
```hs
map (^2) [1..10]
```
- b) 
```hs
map minimum [[1..10], [10..20], [20..30]] -- n.b. minimum is not the same function -- as the min function that we used before
```
- c) 
```hs
map sum [[1..5], [1..5], [1..5]]
```

---

- a) `[1,4,9,16,25,36,49,64,81,100]`
- b) `[1,10,20]`
- c) `[15,15,15]`


### 6. 
Back in Chapter 7, you wrote a function called `foldBool`. That function exists in a module known as `Data.Bool` and is called `bool`. Write a function that does the same (or similar, if you wish) as the `map if-then-else` function you saw above but uses `bool` instead of the `if-then-else` syntax. Your first step should be bringing the `bool` function into scope by typing `import Data.Bool` at your REPL prompt.

`map (\x -> if x == 3 then (-x) else (x)) [1..10]`

---

```hs
map (\x -> bool x (-x) (x == 3) ) [1..10]
```



## Exercises: Filtering (pg.335)

### 1. 
Given the above, how might we write a filter function that would give us all the multiples of `3` out of a list from `1–30`?

---

```hs
filter (\x -> x `rem` 3 == 0) [1..30]
-- [3,6,9,12,15,18,21,24,27,30]
```

### 2. 
Recalling what we learned about function composition, how could we compose the above function with the length function to tell us how many multiples of 3 there are between 1 and 30?

---

```hs
length . filter (\x -> x `rem` 3 == 0) $ [1..30]
-- 10
```

### 3. 
Next, we’re going to work on removing all articles (“the,” “a,” and “an”) from sentences. You want to get to something that works
like this:
```
Prelude> myFilter "the brown dog was a goof"
["brown","dog","was","goof"]
```

You may recall that earlier in this chapter, we asked you to write a function that separates a string into a list of strings by separating them at spaces. That is a standard library function called words. You may consider starting this exercise by using words (or your own version, of course).

---

```hs
myFilter :: String -> [String]
myFilter s = filter ((/=) "the") . filter ((/=) "a") . filter ((/=) "and") . words $ s
```



## Zipping exercises (pg.337)

### 1. 
Write your own version of `zip`, and ensure it behaves the same as the original:

```hs
zip :: [a] -> [b] -> [(a, b)] 
zip = undefined
```

---

```hs
myZip [] _ = []
myZip _ [] = []
myZip xs1 xs2 =
  let
    len = min (length xs1) (length xs2)
    xs = [0..len-1]
  in
    map (\i -> (xs1!!i, xs2!!i)) xs
```

### 2. 
Do what you did for `zip` but now for `zipWith`:

```hs
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined
```

---

```hs
myZipWith f [] xs2 = []
myZipWith f xs1 [] = []
myZipWith f xs1 xs2 = 
  let
    len = min (length xs1) (length xs2)
    xs = [0..len-1]
  in
    map (\i -> f (xs1!!i) (xs2!!i)) xs
```

### 3.
Rewrite your `zip` in terms of the `zipWith` you wrote.

---

```hs
myZip = myZipWith (,)
```



## 9.12 Chapter exercises (pg.338)

### 1. 
Query the types of isUpper and toUpper.

---

```
λ> import Data.Char
λ> :t isUpper
isUpper :: Char -> Bool
λ> :t toUpper
toUpper :: Char -> Char
```

### 2. 
Given the following behaviors, which would we use to write a function that filters all the uppercase letters out of a String? Write that function such that, given the input `"HbEfLrLxO"`, your function will return `"HELLO"`.
```
Prelude Data.Char> isUpper 'J'
True
Prelude Data.Char> toUpper 'j'
'J'
```

---

I would use `isUpper`:

```hs
f :: String -> String
f = filter isUpper
```

### 3.
Write a function that will capitalize the first letter of a string and return the entire string. For example, if given the argument `"julie"`, it will return `"Julie"`.

---

```hs
capitalize :: String -> String
capitalize (x:xs) = (toUpper x) : xs
```

### 4. 
Now make a new version of that function that is recursive, such that if you give it the input `"woot"`, it will holler back at you `"WOOT"`. The type signature won’t change, but you will want to add a base case.

---

```hs
upperize :: String -> String
upperize [] = ""
upperize (x:xs) = (toUpper x) : (upperize xs)
```

### 5. 
To do the final exercise in this section, we’ll need another standard function for lists called `head`. Query the type of `head`, and experiment with it to see what it does. Now write a function that will capitalize the first letter of a `String` and return only that letter as the result.

---

```hs
headCap :: String -> Char
headCap s = toUpper . head $ s
```

### 6. 
Cool. Good work. Now rewrite it as a composed function. Then, for fun, rewrite it point-free.

---

_It was composed already_

```hs
headCap = toUpper . head
```

## Ciphers (pg.339)


```hs
module Cipher (basicCaesarCipher, basicCaesarDecipher) where

import Data.Char

-- Generic encoder/decoder

cipher :: (Char -> Char) -> String -> String
cipher fn s = map fn s


-- Basic Caesar cipher algorithm, shifted 3 chars

rShift = 1
ordA = 97
azIndex ch =  ord ch - ordA
ch_a = 0
ch_z = 25
az = ['a'..'z']

basicEncoder :: Char -> Char
basicEncoder ' ' = ' '
basicEncoder ch = 
  let
    nextChar = azIndex ch + rShift 
    wrapped = nextChar `mod` (ch_z+1)
  in
    az !! wrapped

basicDecoder :: Char -> Char
basicDecoder  ' ' = ' '
basicDecoder ch = 
  let
    prevChar = azIndex ch - rShift 
    wrapped = 
      if prevChar < ch_a
      then (ch_z+1) + prevChar
      else prevChar
  in
    az !! wrapped

-- Public 

basicCaesar = cipher basicEncoder
basicUncaesar = cipher basicDecoder
```



## Writing your own standard functions (pg.340)

### 1. 
`myOr` returns `True` if any `Bool` in the list is `True`:
```hs
myOr :: [Bool] -> Bool 
myOr = undefined
```

---

```hs
myOr [] = False
myOr (x:xs) = x || myOr xs
```

### 2. 
`myAny` returns True if `a -> Bool` applied to any of the values in the list returns `True`:
```hs
myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined
```

Example for validating `myAny`:

```
Prelude> myAny even [1, 3, 5]
False
Prelude> myAny odd [1, 3, 5]
True
```

---

```hs
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs
```

### 3.
After you write the recursive `myElem`, write another version that uses `any`.

```hs
myElem :: Eq a => a -> [a] -> Bool
```

---

```hs
-- Recursive
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) 
  | e == x = True 
  | otherwise = myElem e xs
```

```hs
-- With `any`
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e xs = any ((==) e) xs
```

### 4. 
Implement `myReverse`: 
```hs
myReverse :: [a] -> [a]
myReverse = undefined
```

```
Prelude> myReverse "blah"
"halb"
Prelude> myReverse [1..5]
[5,4,3,2,1]
```

---

```hs
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
```

### 5. 
`squish` flattens a list of lists into a list: 
```hs
squish :: [[a]] -> [a]
squish = undefined
```

---

```hs
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs
```

### 6. 
`squishMap` maps a function over a list and concatenates the results:

```hs
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined
```

```
Prelude> squishMap (\x -> [1, x, 3]) [2]
[1,2,3]
Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
"WO 1 HOO WO 2 HOO WO 3 HOO "
```

---

```hs
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs
```

### 7.
`squishAgain` flattens a list of lists into a list. This time, re-use the `squishMap` function:
```hs
squishAgain :: [[a]] -> [a] 
squishAgain = undefined
```

---

```hs
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = squishMap (\x-> [x]) x ++ squishAgain xs
```

### 8. 
`myMaximumBy` takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returns `GT` for. If you import `maximumBy` from `Data.List`, you’ll see that the type is:

```hs
Foldable t => (a -> a -> Ordering) -> t a -> a
```

Rather than:

```hs
(a -> a -> Ordering) -> [a] -> a
```

```hs
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined
```

```
Prelude> xs = [1, 53, 9001, 10]
Prelude> myMaximumBy compare xs
9001
```

---

```hs
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "you must provide at least one element"
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = 
  let
    gt  = filter (\e -> f e x == GT) xs
  in
    if length gt == 0 -- there were only `x` duplicates, so the filter operation removed everyhitng from xs
    then x
    else myMaximumBy f gt
```

### 9. 
`myMinimumBy` takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returns `LT` for:

```hs
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined
```

```
Prelude> myMinimumBy compare [1, 53, 9001, 10]
1
```

---

```hs
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "you must provide at least one element"
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = 
  let
    lt  = filter (\e -> f e x == LT) xs
  in
    if length lt == 0 -- there were only `x` duplicates, so the filter operation removed everyhitng from xs
    then x
    else myMinimumBy f lt
```

### 10. 
Using the `myMinimumBy` and `myMaximumBy` functions, write your own versions of `maximum` and `minimum`.

```hs
myMaximum :: (Ord a) => [a] -> a 
myMaximum = undefined
myMinimum :: (Ord a) => [a] -> a 
myMinimum = undefined
```

---

```hs
myMaximum :: (Ord a) => [a] -> a 
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a 
myMinimum xs = myMinimumBy compare xs
```
