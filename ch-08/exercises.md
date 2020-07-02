# Recursion Exercises (ch.8)

## Intermission: Exercise (pg.282)

Write out the evaluation of the following. It might be a little less noisy if you do so with the form that doesn’t use the composition operator, `(.)`:

```hs
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)
```

```hs
applyTimes 5 (+1) 5
```

---

```hs
applyTimes 5 (+1) 5 
  = (+1) -- f . (applyTimes 5 (+) 5)
  . (+1) -- f . (applyTimes 4 (+) 5)
  . (+1) -- f . (applyTimes 3 (+) 5)
  . (+1) -- f . (applyTimes 2 (+) 5)
  . (+1) -- f . (applyTimes 1 (+) 5)
  $ 5 ----- f . (applyTimes 0 (+) 5)
```



## 8.6 Chapter Exercises (pg.293)

Review of types

### 1. 
What is the type of `[ [True, False], [True, True], [False, True] ]`?
- a) `Bool`
- b) mostly `True`
- c) `[a]`
- d) `[[Bool]]`

---

It's: **d) `[[Bool]]`**.

### 2. 
Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?
- a) `[(True, False), (True, True), (False, True)]` 
- b) `[[3 == 3], [6 > 5], [3 < 4]]`
- c) `[3 == 3, 6 > 5, 3 < 4]`
- d) `["Bool", "more Bool", "Booly Bool!"]`

---

It's: **b) `[[3 == 3], [6 > 5], [3 < 4]]`**, because the type for both is `[[Bool]]`.


### 3. 
For the function below, which of the following statements are true?

```hs
func :: [a] -> [a] -> [a] 
func x y = x ++ y
```

- a) `x` and `y` must be of the same type. 
- b) `x` and `y` must both be lists.
- c) If `x` is a `String`, then `y` must be a `String`. 
- d) All of the above.

---

It's: **d) All of the above**.

### 4.
For the `func` code above, which is a valid application of `func` to both of its arguments?

- a) `func "Hello World"` 
- b) `func "Hello" "World"`
- c) `func [1, 2, 3] "a, b, c"` 
- d) `func ["Hello", "World"]`

---

Its: **b) `func "Hello" "World"`**.


## Reviewing currying (pg.294)

Given the following definitions, tell us what value results from further applications:

```hs
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy = flip cattyConny
appedCatty = cattyConny "woops" 
frappe = flippy "haha"
```

---

```hs
flippy :: String -> String -> String
```

```hs
appedCatty :: String -> String
```

```hs
frappe :: String -> String
```

### 1.
What is the value of `appedCatty "woohoo!"`?

---

`"woops mrow woohoo!"`

### 2. 
What is the value of `frappe "1"`?

---

`"1 mrow haha"`

### 3.
What is the value of `frappe (appedCatty "2")`?

---

`"1 mrow woops mrow 2"`

### 4.
What is the value of `appedCatty (frappe "blue")`?

---

`"woops mrow blue mrow haha "`

### 5.
What is the value of: 
```hs
cattyConny 
  (frappe "pink")
  (cattyConny "green" (appedCatty "blue"))
```
?

---

1. `appedCatty "blue"` -> "woops mrow blue"
2. `cattyConny "green" "woops mrow blue"` -> "green mrow woops mrow blue"
3. `frappe "pink"` -> "pink mrow haha"

The value is: `"pink mrow haha mrow green mrow woops mrow blue"`

### 6.
What is the value of: 
```hs
cattyConny (flippy "Pugs" "are") "awesome"
```

---

1. `flippy "Pugs" "are"` -> "are mrow Pugs"

The value is: `"are mrow Pugs mrow awesome"`

## Recursion (pg.295)

### 1. 
Write out the steps for reducing `dividedBy 15 2` to its final answer according to the Haskell code.
```hs
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom 
  = go num denom 0
  where 
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)
```

---

```hs
dividedBy 15 2 = 
  --    [n] [d] [count]
  -> go 15  2   0   -------->   n < d == False, so otherwise
  -> go 13  2   1   -------->   n < d == False, so otherwise
  -> go 11  2   2   -------->   n < d == False, so otherwise
  -> go 9   2   3   -------->   n < d == False, so otherwise
  -> go 7   2   4   -------->   n < d == False, so otherwise
  -> go 5   2   5   -------->   n < d == False, so otherwise
  -> go 3   2   6   -------->   n < d == False, so otherwise
  -> go 1   2   7   -------->   n < d == True, so (count, n)
  = (7, 1)  
```

### 2. 
Write a function that recursively sums all numbers from `1` to `n`, `n` being the argument. So if `n` is `5`, you’d add `1+2+3+4+5` to get `15`. The type should be `(Eq a, Num a) => a -> a`.

--

```hs
rSum :: (Eq a, Num a) => a -> a
rSum 0 = 0
rSum n = n + rSum (n-1)
```

### 3. 
Write a function that multiplies two integral numbers using recursive summation. The type should be `(Integral a) => a -> a -> a`.

---

```hs
mul :: (Integral a) => a -> a -> a
mul n 0 = 0
mul n times = n + (mul n (times-1))
```



## Fixing dividedBy (pg.295)

Our `dividedBy` function wasn’t quite ideal. For one thing, it is a partial function and doesn’t return a result (bottom) when given a divisor that is `0` or less.

---

```hs
{-
------------------------------------
dividedBy supporting: 
-> `num` and `denom` less than `0`.
-> `denom` equal to `0`.
------------------------------------
-}

data DividedResult 
  = Result Integer
  | DividedByZero deriving Show

signFn :: (Ord a, Num a, Num b) => a -> b -> b
signFn n = if n < 0 then (* (-1)) else (*1)

go n d count
  | n < d = (count, n)
  | otherwise = go (n - d) d (count + 1)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom
  | denom /= 0 = 
    let 
      res = fst $ go (abs num) (abs denom) 0
    in 
      Result (signFn num . signFn denom $ res)
  | otherwise = DividedByZero
```



## McCarthy 91 function (pg.296)

The **McCarthy 91** function yields `x - 10` when `x > 100` and `91` otherwise. The function is recursive.

```
        ⎧ n − 10             if n > 100
MC(n) = ⎨
        ⎩ MC(MC(n + 11))     if n <= 100
```

---

```hs
mc91 :: (Ord a, Num a) => a -> a
mc91 n
  | n > 100    = n - 10
  | otherwise  = mc91 $ mc91 $ n + 11
```




## Numbers in words

Implement the functions below so:

```
Prelude> wordNumber 12324546
"one-two-three-two-four-five-four-six"
```

```hs
module WordNumber where
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = undefined

digits :: Int -> [Int]
digits n = undefined

wordNumber :: Int -> String
wordNumber n = undefined
```

---

```hs
digitToWord :: Int -> String
digitToWord n
  | n >= 0 && n < 10 = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! n
  | otherwise        = error "n must be between 0 and 9."

-- there's proof of the execution in the wordNumber.hs file
digits :: Int -> [Int]
digits num = 
  numAcc []
  where
    numAcc xs = 
      let
        divisor = 10 ^ (length xs)
        digit = num `div` divisor `mod` 10
      in
        if divisor <= num
        then numAcc $ digit : xs
        else xs

(|>) = flip ($)

wordNumber :: Int -> String
wordNumber n 
  = digits n
  |> map digitToWord
  |> intersperse "-"
  |> concat
```
