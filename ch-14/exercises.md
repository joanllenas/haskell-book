## Intermission: Short exercise (pg.538)

In the Chapter Exercises at the end of Chapter 8, you were given this exercise:

Write a function that multiplies two numbers using recursive summation. The type should be `(Eq a, Num a) => a -> a -> a`, although, depending on how you do it, you might also consider adding an `Ord` constraint.

If you still have your answer, great! If not, rewrite it, and then write hspec tests for it.

The above examples demonstrate the basics of writing individual tests to test particular values. If you’d like to see a more developed example, you could refer to Chris’s library, [Bloodhound](https://github.com/bitemyapp/bloodhound).

---

```hs
-- see the recursive-summation project.
```



## 14.7 Chapter exercises (pg.564)

## Validating numbers into words

Remember the “numbers into words” exercise in Chapter 8? You’ll be writing tests to validate the functions you wrote:

---

```hs
-- see the word-number project
```




### Using QuickCheck (pg.566)

Test some basic properties using QuickCheck:

### 1. 

```hs
-- for a function
half x = x / 2
-- this property should hold
halfIdentity = (*2) . half
```

---

```hs
half :: Fractional a => a -> a
half x = x / 2

ex1 :: IO ()
ex1 = hspec $ do
  describe "half" $ do
    it "half x * 2 is always x" $ do
      property $ \x ->  x == ((*2) . half $ x::Float)
```

### 2. 
```hs
import Data.List (sort)
-- for any list you apply sort to,
-- this property should hold 
listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs =
  snd $ foldr go (Nothing, True) xs 
  where go _ status@(_, False) = status 
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)
```

---

```hs
ex2 :: IO ()
ex2 = hspec $ do
  describe "sort" $ do
    it "listOrdered $ sort x should always be true" $ do
      property $ (\x -> (listOrdered $ sort (x::[Int])) == True)
-- I tried creating a gen for Ord, but it was impossible..
```

### 3. 
Now, we’ll test the associative and commutative properties of addition:

```hs
plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x
```

Keep in mind, these properties won’t hold for types based on IEEE-754 floating point numbers, such as `Float` or `Double`.

---

```hs
ex3 :: IO ()
ex3 = hspec $ do
  describe "+ associativity" $ do
    it "parenthesization doesn't matter" $ do
      property $ (\x y z -> (plusAssociative (x::Int) y z) == True)
  describe "+ commutative" $ do
    it "order doesn't matter" $ do
      property $ (\x y -> (plusCommutative (x::Int) y) == True)
```

### 4. 
Now do the same for multiplication.

---

```hs
ex4 :: IO ()
ex4 = hspec $ do
  describe "* associativity" $ do
    it "parenthesization doesn't matter" $ do
      property $ (\x y z -> (mulAssociative (x::Int) y z) == True)
  describe "* commutative" $ do
    it "order doesn't matter" $ do
      property $ (\x y -> (mulCommutative (x::Int) y) == True)
```

### 5. 
We mentioned in one of the first chapters that there are some laws involving the relationships of `quot` to `rem` and `div` to `mod`. Write QuickCheck tests to prove them:

```hs
(quot x y) * y + (rem x y) == x 
(div x y) * y + (mod x y) == x
````

---

```hs
quotRemProp :: Integer -> NonZero Integer -> Bool
quotRemProp x (NonZero y) = quotRemProof x y

divModProp :: Integer -> NonZero Integer -> Bool
divModProp x (NonZero y) = divModProof x y

ex5 :: IO ()
ex5 = hspec $ do
  describe "quot rem law" $ do
    it "should comply with the law" $ do
      property $ quotRemProp
  describe "div mod law" $ do
    it "should comply with the law" $ do
      property $ divModProp
```

### 6. 
Is the `^` operation associative? Is it commutative? Use QuickCheck
to see if the computer can contradict such an assertion.

---

```hs
genx :: Gen Integer
genx = choose (25, 200)
geny :: Gen Integer
geny = choose (2, 4) -- bigger numbers hang ghci
genz :: Gen Integer
genz = choose (5, 7) -- bigger numbers hang ghci

genExpTuple :: Gen (Integer, Integer, Integer)
genExpTuple = do
  x <- genx
  y <- geny
  z <- genz
  return (x, y, z)

expNotAssociative :: Integer -> Integer -> Integer -> Bool
expNotAssociative x y z = x ^ (y ^ z) /= (x ^ y) ^ z

expNotCommutative :: Integer -> Integer -> Bool
expNotCommutative x y = x ^ y /= y ^ x
```

### 7. 
Test that reversing a list twice is the same as the identity of the original list:

```hs
reverse . reverse == id
```

---

```hs
listReverseProp :: [Int] -> Bool
listReverseProp xs = (reverse . reverse $ xs) == xs

ex7 :: IO ()
ex7 = hspec $ do
  describe "list" $ do
    it "reversing xs twice equals xs" $ do
      property $ listReverseProp
```

### 8. 
Write a property for the definition of $:

```hs
f $ a = f a
f . g = \x -> f (g x)
```

---

```hs
dollarProp :: (Eq b) => (a -> b) -> a -> Bool
dollarProp f a = (f $ a) == f a

dotProp :: (Eq c) => (a -> b) -> (b -> c) -> a -> Bool
dotProp g f x = (f . g $ x) == f (g x)

ex8 :: IO ()
ex8 = hspec $ do
  describe "$" $ do
    it "f $ x and f x are the same" $ do
      property $ \x -> dollarProp (+1) (x::Int)
  describe "f . g $ x and f (g x) are the same" $ do
    it ". and function application are the same" $ do
      property $ \x -> dotProp (+20) (+3) (x::Int)
```

### 9. 
See if these two functions are equal:

```hs
foldr (:) == (++)
foldr (++) [] == concat
```

---

```hs
-- don't understand
```


### 10. 
Hmm. Is that so?

```hs
f n xs = length (take n xs) == n
```

---

```hs
f :: (Positive Int) -> [a] -> Bool
f (Positive n) xs = length (take n xs) == n

ex10 :: IO ()
ex10 = hspec $ do
  describe "f" $ do
    it "length (take n xs) and n are the same" $ do
      property $ \n -> f n $ enumFrom (0::Int)
```

### 11. 
Finally, this is a fun one. You may remember we had you compose `read` and `show` one time to complete a “round trip.” Well, now you can test that it works:

```hs
f x = (read (show x)) == x
```

---

```hs
fn11 :: (Eq a, Read a, Show a) => a -> Bool
fn11 x = (read (show x)) == x

ex11 :: IO ()
ex11 = hspec $ do
  describe "fn11" $ do
    it "read show x and x are the same" $ do
      property $ \x -> fn11 (x::Char)
```



## Failure
Find out why this property fails:

```hs
-- for a function
square x = x * x
-- Why does this property not hold? 
-- Examine the type of sqrt. 
squareIdentity = square . sqrt
```

Hint: Read about floating point arithmetic and precision if you’re unfamiliar with it.

---

```hs
square :: Num a => a -> a
square x = x * x

squareIdentity :: Double -> Double
squareIdentity = square . sqrt

squapreProp :: Double -> Bool
squapreProp x = squareIdentity x == square x

exFailure :: IO ()
exFailure = verboseCheck squapreProp

{-
λ> verboseCheck squapreProp
Passed:  
0.0
Failed: 
0.7948723611274879
Failed:                                                 
0.4
Failed:                                                 
0.2
...
-}
```

> The proof fails because floating point arithmetic has finite precision and error accumulates over the operations:

```
λ> sqrt (0.2::Double)
0.4472135954999579

λ> square (0.4472135954999579::Double)
0.19999999999999998
```

> Interesting read about this: [Taming Floating Point Error](https://www.johnbcoughlin.com/posts/floating-point-axiom/)



## Idempotence
Use QuickCheck and the following helper functions to demonstrate idempotence for the following:

```hs
twice f = f . f
fourTimes = twice . twice
```

### 1. 
```hs
f x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)
```

### 2. 
```hs
f' x = (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)
```

---

```hs
twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

--

capitalizeWord :: String -> String 
capitalizeWord "" = ""
capitalizeWord (x:xs) = (Char.toUpper x) : xs

--

genLetter :: Gen Char
genLetter = choose ('a', 'z')

genWord :: Gen String
genWord = listOf1 genLetter -- listOf1 is non empty list

--

fnIdmp :: String -> Bool
fnIdmp x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

fnIdmp' :: (Ord a) => [a] -> Bool
fnIdmp' x = (List.sort x
  == twice List.sort x)
  &&
  (List.sort x
  == fourTimes List.sort x)

-- 1.
exIdempotence :: IO ()
exIdempotence = verboseCheck $ forAll genWord $ fnIdmp

-- 2.
exIdempotence' :: IO ()
exIdempotence' = verboseCheck $ forAll genWord $ fnIdmp'
```



## Make a Gen random generator for the datatype
We demonstrate in this chapter how to make Gen generators for different datatypes. We are so certain you enjoyed that, we are going to ask you to do it for some new datatypes:

```hs
data Fool 
  = Fulse
  | Frue
  deriving (Eq, Show)
```

### 1. 
Equal probabilities for `Fulse` and `Frue`.

### 2. 
`2/3`s chance of `Fulse`, `1/3` chance of `Frue`.

---

```hs
-- 1.
foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

-- 2.
foolGen' :: Gen Fool
foolGen' 
  = frequency 
  [ (7, return Fulse) -- round (2/3 * 10) = 7
  , (4, return Frue)] -- round (1/3 * 10) = 3
```
