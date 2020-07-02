# Exercieses



## Grab Bag (pg.227)

> Note that the following exercises are from source code files, not written for use directly in the REPL. Of course, you can change them to test directly in the REPL, if you prefer.

### 1. 
Which (two or more) of the following are equivalent?
- a) `mTh x y z = x * y * z`
- b) `mTh x y = \z -> x * y * z`
- c) `mTh x = \y -> \z -> x * y * z`
- d) `mTh = \x -> \y -> \z -> x * y * z`

---

All of them are equivalent.

## 2. 
The type of `mTh` (above) is `Num a => a -> a -> a -> a`. Which is the type of `mTh 3`?
- a) `Integer -> Integer -> Integer`
- b) `Num a => a -> a -> a -> a`
- c) `Num a => a -> a`
- d) `Num a => a -> a -> a`

---

It's **d) `Num a => a -> a -> a`** because the first argument has been provided already.

### 3. 
Next, we’ll practice writing anonymous lambda syntax. For example, one could rewrite:

`addOne x = x + 1`

Into:

`addOne = \x -> x + 1`

Try to make it so it can still be loaded as a top-level definition by GHCi. 
This will make it easier to validate your answers.

- a) Rewrite the `f` function in the `where` clause:
```hs
addOneIfOdd n = case odd n of 
  True -> f n
  False -> n
  where f n = n + 1
```

---

```hs
addOneIfOdd = \n -> case odd n of 
  True -> f n
  False -> n
  where f n = n + 1
```

- b) Rewrite the following to use anonymous lambda syntax:
```hs
addFive x y = (if x > y then y else x) + 5
```

---

```hs
addFive = \x y -> (if x > y then y else x) + 5
```

- c) Rewrite the following so that it doesn’t use anonymous
lambda syntax:
```hs
mflip f = \x -> \y -> f y x
```

---

```hs
mflip f x y = f y x
```



## Exercises: Variety pack (pg.237)

### 1.
Given the following declarations:
```hs
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)
```
- a) What is the type of `k`?
- b) What is the type of `k2`? Is it the same type as `k1` or `k3`?
- c) Of `k1`, `k2`, `k3`, which will return the number `3` as the result?

---

- a) `k :: (a, b) -> a`.
- b) `k2 :: [Char]`. Not the same as `k1` or `k3`.
- c) `k1` and `k3`.

### 2. 
Fill in the definition of the following function:

```hs
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f = undefined
```

Remember that tuples have the same syntax for their type constructors and their data constructors.

---

```hs
f (a, b, c) (d, e, f) = ((a, d), (c, f))
```



## Exercises: Case practice (pg.240)

We’re going to practice using case expressions by rewriting functions. Please note, these are all written as they would be in source code files, and we recommend you write your answers in source files, and then load them into GHCi to check, rather than trying to do them directly in the REPL.

First, rewrite if-then-else expressions into case expressions. 

### 1. 
The following should return `x` when `x` is greater than `y`:

```hs
functionC x y = if (x > y) then x else y
```
---
```hs
functionC x y = case x > y of
  True -> x
  False -> y
```

### 2.
The following will add `2` to even numbers and otherwise simply return the input value:
```hs
ifEvenAdd2 n = if even n then (n+2) else n
```
---

```hs
ifEvenAdd2 n = case even n of
  True -> (n+2)
  False -> n
```
The next exercise doesn’t have all the cases covered. See if you can fix it.

### 3.
The following compares a value, `x`, to `0` and returns an indicator for whether `x` is a positive number or negative number. What if `x` is `0`? You may need to play with the compare function a bit to find what to do:

```hs
nums x =
  case compare x 0 of
    LT -> -1 
    GT -> 1
```

---

```hs
nums x =
  case compare x 0 of
    LT -> -1 
    GT -> 1
    EQ -> 0
```



## Exercises: Artful dodgy (pg.248)

Given the following definitions, tell us what value results from further applications. When you’ve written down at least some of the answers and think you know what’s what, type the definitions into a file, and load them in GHCi to test your answers:

```hs
-- Types not provided,
-- try filling them in yourself.

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
```

---

Types:

```hs
dodgy :: Num a => a -> a -> a; dodgy x y = x + y * 10
oneIsOne :: Num a => a -> a; oneIsOne = dodgy 1
oneIsTwo :: Num a => a -> a; oneIsTwo = (flip dodgy) 2
```

### 1.
```dodgy 1 0```

---
`1`

### 2.
```dodgy 1 1 ```

---
`11`

### 3. 
```dodgy 2 2```

---
`22`

### 4. 
```dodgy 1 2``` 

---
`21`

### 5. 
```dodgy 2 1``` 

---
`12`

### 6. 
```oneIsOne 1```

---
`1 + 1 * 10`
`11`

### 7. 
```oneIsOne 2``` 

---

`1 + 2 * 10`
`21`

### 8. 
```oneIsTwo 1``` 

---

`1 + 2 * 10`
`21`

### 9. 
```oneIsTwo 2```

---

`2 + 2 * 10`
`22`

### 10. 
```oneIsOne 3``` 

---

`1 + 3 * 10`
`31`

### 11. 
```oneIsTwo 3```

---

`3 + 2 * 10`
`23`




# Exercises: Guard duty (pg.254)

```hs
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x 
  | y >= 0.9 = 'A' 
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100
```

### 1. 
It is probably clear to you why you wouldn’t put an otherwise in your top-most guard, but try it with `avgGrade` anyway, and see what happens. It’ll be clearer if you rewrite it as an otherwise match: `| otherwise = 'F'`. What happens now if you pass `90` as an argument? `75`? `60`?

---
- `90`: _(y=0.9)_ `'A'`
- `75`: _(y=0.75)_ `'C'`
- `60`: _(y=0.6)_ `'D'`

### 2. 
What happens if you take `avgGrade` as it is written and reorder the guards? Does it still type check and work the same way? Try moving `| y >= 0.7 = 'C'` and passing it the argument `90`, which should be an `'A'`. Does it return `'A'`?

---

- It still type checks.
- It doesn't work the same way. If you move `| y >= 0.59 = 'D'` to the top, `'A'`, `'B'` and `'C'` would be impossible to match, because `y >= 0.59` will match first and resume the guard execution.
- _Move where? the exercise is not precise enough._

### 3. 
What does the following function return?

```hs
pal xs
  | xs == reverse xs = True 
  | otherwise = False
```

- a) `xs` written backwards when it’s `True`.
- b) `True` when `xs` is a palindrome.
- c) `False` when `xs` is a palindrome.
- d) `False` when `xs` is reversed.

---

- **b) `True` when `xs` is a palindrome.**

### 4. 
What types of arguments can `pal` take?

---

- Can the same param `reverse` takes, which is `[a]`.

### 5. 
What is the type of the function `pal`?

---

```hs
pal :: [a] -> Bool
```

### 6. 
What does the following function return?

```hs
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
```

- a) The value of its argument plus or minus `1`. 
- b) The negation of its argument.
- c) An indication of whether its argument is a positive or negative number or `0`.
- d) Binary machine language.

---

- **c) An indication of whether its argument is a positive or negative number or `0`.**

### 7. 
What types of arguments can `numbers` take?

---

- Numeric types.

### 8.
What is the type of the function `numbers`?

---

```hs
numbers :: (Ord a, Num a) => a -> Int
```




## (self note) 

### Function composition (.)

Instead of right to left:

```hs
take 5 . filter odd . enumFrom $ 3
```

you think of function application from left to right:

```hs
(>>>) = flip (.)
res 
  = enumFrom 
  >>> filter odd 
  >>> take 5 
  $ 3
```

### Function pipeline ($)

Same with `($)`:

Instead of right to left:

```hs
take 5 $ filter odd $ enumFrom $ 3
```

we can do it in logical order:

```hs
(|>) = flip ($)
res 
  = 3 
  |> enumFrom 
  |> filter odd 
  |> take 5 
```

> Like in Elm or F#




## 7.11 Chapter exercises (pg.264)

### Multiple choice

### 1. 
A polymorphic function:
- a) Changes things into sheep when invoked.
- b) Has multiple arguments.
- c) Has a concrete type.
- d) May resolve to values of different types, depending on inputs.

---

It's: **d) May resolve to values of different types, depending on inputs.**.

### 2. 
Two functions named `f` and `g` have types `Char -> String` and `String -> [String]`, respectively. The composed function `g . f` has the type:
- a) `Char -> String 
- b) `Char -> [String]`
- c) `[[String]]`
- d) `Char -> String -> [String]`

---

It's: **b) `Char -> [String]`**. Function application starts at `f`, which takes a `Char`, and ends at `g`, which returns a `[String]`.

---

### 3.
A function `f` has the type `Ord a => a -> a -> Bool`,and we apply it to one numeric value. What is the type now?
- a) `Ord a => a -> Bool` 
- b) `Num -> Num -> Bool`
- c) `Ord a => a -> a -> Integer` 
- d) `(Ord a, Num a) => a -> Bool`

---

It's **d) `(Ord a, Num a) => a -> Bool`**, because the numeric value applied has narrowed the `Ord a` type to the `Num` typeclass.

### 4. 
A function with the type `(a -> b) -> c`:
- a) Requires values of three different types. 
- b) Is a higher-order function.
- c) Must take a tuple as its first argument.
- d) Has its parameters in alphabetical order.

---

It's **b) Is a higher-order function**, because takes a function as argument.

### 5. 
Given the following definition of `f`, what is the type of `f True`? 

```hs
f :: a -> a
f x = x
```

- a) `f True :: Bool` 
- b) `f True :: String`
- c) `f True :: Bool -> Bool` 
- d) `f True :: a`

---

It's: **a) `f True :: Bool`**, because `True` narrows `a` return type to the `Bool` typeclass.



## Let's write code (pg.265)

### 1. 

The following function returns the tens digit of an integral argument:

```hs
tensDigit :: Integral a => a -> a 
tensDigit x = d
  where 
    xLast = x `div` 10
    d = xLast `mod` 10
```

- a) First, rewrite it using divMod.
- b) Does the divMod version have the same type as the original version?
- c) Next, let’s change it so that we’re getting the hundreds digit instead. You could start it like this (though that may not be the only possibility):
```hs
hunsD x = d2
  where d = undefined 
  ...
```

---

- a)

```hs
tensDigit' :: Integral a => a -> a
tensDigit' 
  = (flip mod $ 10) 
  . fst 
  . (flip divMod $ 10)
```

- b)

Yes, it has the same type because we return a function `Integral a => a -> a`. Point-free style.

- c)

```hs
hunsD :: Integral a => a -> a
hunsD   
  = (flip mod $ 10) 
  . (flip div $ 100)
```

### 2. 
Implement the following function of the type `a -> a -> Bool -> a` once using a case expression and once with a guard:
```hs
foldBool :: a -> a -> Bool -> a 
foldBool =
  error
  "Error: Need to implement foldBool!"
```
The result is semantically similar to if-then-else expressions but syntactically quite different. Here is the pattern matching version to get you started:
```hs
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y
```

---

```hs
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y bool = 
  case bool of
    True -> y
    False -> x

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y bool
  | bool == True = y
  | bool == False = x
```


### 3. 
Fill in the definition. Note that the first argument to our function is also a function that can be applied to values. Your second argument is a tuple, which can be used for pattern matching:

```hs
g :: (a -> b) -> (a, c) -> (b, c) 
g = undefined
```

---

```hs
g :: (a -> b) -> (a, c) -> (b, c) 
g a2b (a, c) = (a2b a, c)

-- Usage:
-- λ> g show (1, 2)
-- ("1",2)
```

### 4. 
For this next exercise, you’ll experiment with writing point-free versions of existing code. This involves some new information, so read the following explanation carefully.

Type classes are dispatched by type. `Read` is a type class like `Show`, but it is the dual or “opposite” of `Show`. In general, the `Read` type class isn’t something you should plan to use, but this exercise is structured to teach you something about the interaction between type classes and types.

The function `read` in the `Read` type class has the type:

```hs
read :: Read a => String -> a
```

Notice a pattern?

```hs
read :: Read a => String -> a
show :: Show a => a -> String
```

Type the following code into a source file. Then load it, and run it in GHCi to make sure you understand why the evaluation results in the answers you see:

```hs
-- arith4.hs
module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4) 
  print (id 4)
```

---

roundTrip flow:
- `show 4` converts the number `4` into the string `"4"`.
- `read` recives `"4"` and converts it back to `4`.
- `print` recives `4` and converts it into `"4"` again.
- Finally, `IO` does _something_ to print `"4"` to the console.

### 5. 
Next, write a point-free version of `roundTrip`. (n.b., this refers to the function definition, not to its application in main.)

---

```hs
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show
```

### 6. 
We will continue to use the code in module "Arith4" for this exercise, as well.

When we apply `show` to a value such as `(1 :: Int)`, the `a` that implements `Show` is type `Int`, so GHC will use the `Int` instance of the `Show` type class to stringify our `Int` value `1`.

However, `read` expects a `String` argument in order to return an `a`. The `String` argument that is the first argument to `read` tells the function nothing about what type the de-stringified result should be. In the type signature `roundTrip` currently has, it knows, because the type variables are the same, so the type that is the input to show has to be the same type as the output of `read`.

Your task now is to change the type of `roundTrip` in "Arith4" to `(Show a, Read b) => a -> b`. How might we tell GHC which instance of `Read` to dispatch against the `String`? Make the expression `print (roundTrip 4)` work. You will only need the _has the type_ syntax of `::` and parentheses for scoping.

---

```hs
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

-- Usage:
-- λ> roundTrip' 4::Int
-- 4
```

