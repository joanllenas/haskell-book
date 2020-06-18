# Exercises

## Type Matching

Below you’ll find a list of several standard functions we’ve talked about previously. Under that is a list of their type signatures. Match the function to its type signature.


### Functions: 
- a) `not`
- b) `length`
- c) `concat`
- d) `head`
- e) `(<)`

### Type signatures: 

- A) `_ :: [a] -> a`
- B) `_ :: [[a]] -> [a]`
- C) `_ :: Bool -> Bool`
- D) `_ :: [a] -> Int`
- E) `_ :: Ord a => a -> a -> Bool`

---

- a) with C)
- b) with D)
- c) with B)
- d) with A)
- e) with E)

## Type Arguments

Given a function and its type, tell us what type results from applying some or all of the arguments.

### 1.
If the type of `f` is `a -> a -> a -> a`, and the type of `𝑥` is `Char` then the type of `f x is` 
- a) `Char -> Char -> Char`
- b) `x -> x -> x -> x`
- c) `a -> a -> a`
- d) `a -> a -> a -> Char`
---
Is: **a) `Char -> Char -> Char`**.


### 2. 
If the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"` is 
- a) `String`
- b) `Char -> String`
- c) `Int`
- d) `Char`
---
Is: **d) `Char`**.

### 3. 
If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1.0 2` is:
- a) `Double`
- b) `Integer`
- c) `Integral b => b`
- d) `Num b => b`
---
~~Is b) `Integer`.~~

Is: **d) `Num b => b`**, because the type will depend on the operation performed on `b` in the function implementation.

### 4. 
If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1 (5.5 :: Double)` is: 
- a) `Integer`
- b) `Fractional b => b`
- c) `Double`
- d) `Num b => b`
---
Is: **c) `Double`**.

### 5. 
If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard" "has the word jackal in it"` is:
- a) `[Char]`
- b) `Eq b => b`
- c) `b -> [Char]`
- d) `b`
- e) `Eq b => b -> [Char]`
---
Is: **a) `[Char]`**, because "keyboard" has type `[Char]`, which is more precise than the constrained polymorphic type `Ord a`.

### 6. 
If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard" `is:

- a) `b`
- b) `Eq b => b`
- c) `[Char]`
- d) `b -> [Char]`
- e) `Eq b => b -> [Char]`
---
Is: **e) `Eq b => b -> [Char]`**, because `"keyboard"` has type `[Char]`, which is more prceise than `Ord a => a` and there's still one last parameter to resolve, so the function is partially applied.

### 7. 
If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel 1 2` is:
- a) `Integer`
- b) `Int`
- c) `a`
- d) `(Num a, Ord a) => a`
- e) `Ord a => a`
- f )` Num a => a`
---
~~Is **e) `Ord a => a`**, because we can't be more specific about `a`'s numeric type until the function implementation dictates it depending on what operation is done with `a`.~~

Is **d) `(Num a, Ord a) => a`**. <-- Why? 
- Is this because, with the provided information the compiler can infer the `a` is `Num` aside from `Ord`? 
- If yes, isn't `Ord` implicit in `Ord`? why not `(Num a) => a` then?

### 8. 
If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel 1 (2 :: Integer)` is:
- a) `(Num a, Ord a) => a`
- b) `Int`
- c) `a`
- d) `Num a => a`
- e) `Ord a => a`
- f) `Integer`
---
Is **a) `(Num a, Ord a) => a`**.

### 9.
If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel (1 :: Integer) 2` is:
- a) `Num a => a`
- b) `Ord a => a`
- c) `Integer`
- d) `(Num a, Ord a) => a`
- e) `a`
---
Is **c) `Integer`**.

## Parametricity

All you can really do with a parametrically polymorphic value is pass or not pass it to some other expression. Prove that to yourself with these small demonstrations.

### 1.
Given the type `a -> a`, which is the type for `id`, attempt to make a function that is not bottom and terminates successfully that does something other than returning the same value. This is impossible, but you should try it anyway. 

--
I tried pattern matching but it's not possible:
```hs
noId :: a -> a
noId (Num x) = x
noId x = x
```

### 2. 
We can get a more comfortable appreciation of parametricity by looking at `a -> a -> a`. This hypothetical function has two–and only two–implementations. Write both possible versions of it. After doing so, try to violate the constraints of parametrically polymorphic values we outlined above.

---

```hs
-- v1
fn1 :: a -> a -> a
fn1 x y => x
-- v2
fn2 :: a -> a -> a
fn2 x y => y
```

I don't know how to do this: `"try to violate the constraints of parametrically polymorphic values we outlined above"` 🤷‍♂️

### 3.
Implement `a -> b -> b`. How many implementations can it have? Does the behavior change when the types of `𝑎` and `𝑏` change?

---

One implementation only:
```hs
fn :: a -> b -> b
fn x y = y
```

The behavior does not change when the types of `𝑎` and `𝑏` change.

## Apply Yourself

Look at these pairs of functions. One function is unapplied, so the compiler will infer maximally polymorphic type. The second function has been applied to a value, so the inferred type signature may have become concrete, or at least less polymorphic. 

Figure out how the type would change and why, make a note of what you think the new inferred type would be and then check your work in GHCi.

> ??? How do you check the result ???

### 1.
```hs
-- Type signature of general function 
(++) :: [a] -> [a] -> [a] 
-- How might that change when we apply 
-- it to the following value? 
myConcat x = x ++ " yo" 
```
---

```hs
(++) :: [Char] -> [Char] -> [Char] 
```

### 2. 
```hs
-- General function 
(*) :: Num a => a -> a -> a 
-- Applied to a value 
myMult x = (x / 3) * 5 
```

---

```hs
(*) :: Fractional a => a -> a -> a 
```

### 3. 
```hs
take :: Int -> [a] -> [a] 
myTake x = take x "hey you" 
```

---

```hs
take :: Int -> [Char] -> [Char]
```

### 4. 
```hs
(>) :: Ord a => a -> a -> Bool 
myCom x = x > (length [1..10])
```
---
```hs
(>) :: (Num a, Ord a) => a -> a -> Bool
```

### 5. 
```hs
(<) :: Ord a => a -> a -> Bool 
myAlph x = x < 'z'
```
---

```hs
(<) :: Char -> Char -> Bool
```

## 5.8 Chapter Exercises

Welcome to another round of “Knowing is not enough; we must apply.”

### Multiple Choice

### 1.
A value of type `[a]` is
- a) a list of alphabetic characters 
- b) a list of lists 
- c) a list whose elements are all of some type `𝑎` 
- d) a list whose elements are all of different types 

---

Is: **c) a list whose elements are all of some type `𝑎`**.

### 2. 
A function of type `[[a]] -> [a]` could 
- a) take a list of strings as an argument 
- b) transform a character into a string
- c) transform a string in to a list of strings 
- d) take two arguments 

---

Is **a) take a list of strings as an argument**.

### 3. 
A function of type `[a] -> Int -> a`
- a) takes one argument 
- b) returns one element of type `𝑎` from a list 
- c) must return an `Int` value 
- d) is completely fictional 
---
Is **b) returns one element of type `𝑎` from a list**.

### 4. 
A function of type `(a, b) -> a` 
- a) takes a list argument and returns a `Char` value 
- b) has zero arguments 
- c) takes a tuple argument and returns the first value 
- d) requires that `𝑎` and `𝑏` be of different types

---

Is **c) takes a tuple argument and returns the first value**.

## Determine the Type

For the following functions, determine the type of the specified value.

Type them in a file and add:
```hs
{-# LANGUAGE NoMonomorphismRestriction #-}
```
to overcome the *monomorphism restriction*.

Do your best to determine the most polymorphic type an expression could have in the following exercises.

### 1.
All function applications return a value. Determine the value returned by these function applications and the type of that value.
- a) `(* 9) 6`
- b) `head [(0,"doge"),(1,"kitteh")]`
- c) `head [(0 :: Integer ,"doge"),(1,"kitteh")]`
- d) `if False then True else False`
- e) `length [1, 2, 3, 4, 5]`
- f) `(length [1, 2, 3, 4]) > (length "TACOCAT")`

---

- a) `Num a => a`
- b) `Num a => (a, [Char])`
- c) `(Integer, [Char])`
- d) `Bool`
- e) `Int`
- f) `Bool`

### 2.

Given

```hs
x = 5 
y = x + 5 
w = y * 10 
```

What is the type of `w`?

---

`Num a => a`

### 3.

Given

```hs
x = 5 
y = x + 5 
z y = y * 10
```

What is the type of `z`?

---

`Num a => a -> a`

### 4.

Given

```hs
x = 5 
y = x + 5 
f = 4 / y
```

What is the type of `f`?

---

`Fractional a => a`

### 5.

Given

```hs
x = "Julie" 
y = " <3 " 
z = "Haskell" 
f = x ++ y ++ z
```

What is the type of `f`?

---

`[Char]`

## Does it compile?

For each set of expressions, figure out which expression, if any, causes the compiler to squawk at you (n.b. we do not mean literal squawking) and why. Fix it if you can.

### 1.
```hs
bigNum = (^) 5 $ 10 
wahoo = bigNum $ 10
```
---
- `wahoo` makes no sense because `bigNum` is a number, and we can't operate two numbers with `$`.

### 2.
```hs
x = print
y = print "woohoo!"
z = x "hello world"
```
---
- It's fine.

### 3.
```hs
a = (+) 
b = 5 
c = b 10 
d = c 200 
```
---
- `c` makes no sense. You camn't apply `10` to `5`.
- `d` is wrong because `c` is wrong.

### 4. 
```hs
a = 12 + b 
b = 10000 * c
```
---
- `c` is not in scope.

## Type variable or specific type constructor? 

### 1. 
You will be shown a type declaration, and you should categorize each type. The choices are a fully polymorphic type variable, constrained polymorphic type variable, or concrete type constructor.

```hs
f :: Num a => a -> b -> Int -> Int 
--            [0]  [1]  [2]    [3]
```
---
0. Constrained polymorphic.
1. Fully polymorphic.
2. Concrete.
3. Concrete.

### 2.
```hs
f :: zed -> Zed -> Blah
--   [0]    [1]    [2]
```
---
0. Fully polymorphic.
1. Concrete.
2. Concrete.

### 3.
```hs
f :: Enum b => a -> b -> C
--             [0]  [1]  [2]
```
---
0. Fully polymorphic.
1. Constrained polymorphic.
2. Fully polymorphic.

### 4.
```hs
f :: f -> g -> C
--   [0]  [1]  [2]
```
---
0. Fully polymorphic.
1. Fully polymorphic.
2. Concrete. (because it's uppercase `c`)

## Write a type signature

For the following expressions, please add a type signature. You should be able to rely on GHCi type inference to check your work, although you might not have precisely the same answer as GHCi gives (due to polymorphism, etc). 

### 1. 
While we haven’t fully explained this syntax yet, you’ve seen it in Chapter 2 and as a solution to an exercise in Chapter 4. This syntax is a way of destructuring a single element of a list.

```hs
functionH (x:_) = x
```
---
```hs
functionH :: [a] -> a
```

### 2.
```hs
functionC x y = if (x > y) then True else False
```
---
```hs
functionC :: Ord a => a -> a -> Bool
```

### 3.
```hs
functionS (x, y) = y
```
---
```hs
functionS :: (a, b) -> b
```

## Given a type, write the function

You will be shown a type and a function that needs to be written. Use the information the type provides to determine what the function should do. We’ll also tell you how many ways there are to write the function. Syntactically different but semantically equivalent implementations are not counted as being different. For example, writing a function one way then rewriting the semantically identical function but using anonymous lambda syntax does not count as two implementations.

### 1.
```hs
i :: a -> a
```
---
```hs
i x = x
```

### 2.
```hs
c :: a -> b -> a
```
---
```hs
c x y = x
```

### 3.
```hs
c2 :: b -> a -> b
```
---
Yes, they are the same.
```hs
c2 = x y = x
```

### 4.
```hs
c3 :: a -> b -> b
```
---
```hs
c3 x y = y
```

### 5.
```hs
r :: [a] -> [a]
```
---
tail:
```hs
r (x:xs) = xs
```
reverse:
```hs
r :: String -> String
r xs = reverse xs
```

### 6.
```hs
co :: (b -> c) -> (a -> b) -> a -> c
```
---
```hs
co bToc aTob a = bToc $ aTob a
```

### 7.
```hs
a :: (a -> c) -> a -> a
```
---
```hs
a _ x = x
```

### 8.
```hs
a2 :: (a -> b) -> a -> b
```
---
```hs
a2 aTob a = aTob a
```

## Fix it

### 1. 
```hs
module sing where
  fstString :: [Char] ++ [Char] 
  fstString x = x ++ " in the rain" 
  
  sndString :: [Char] -> Char 
  sndString x = x ++ " over the rainbow" 
  
  sing = if (x > y) then fstString x or sndString y 
  where 
    x = "Singin" 
    x = "Somewhere"
```
---
```hs
module Sing where
  fstString :: [Char] -> [Char] 
  fstString x = x ++ " in the rain" 
  
  sndString :: [Char] -> [Char]
  sndString x = x ++ " over the rainbow"
  
  sing = 
    if (x > y) 
      then fstString x 
      else sndString y 
    where 
      x = "Singin" 
      y = "Somewhere"
```

### 2. 
Now that it’s fixed, make a minor change and make it sing the other song. If you’re lucky, you’ll end up with both songs stuck in your head!

---
Just change:
```hs
--(...)
  sing = 
    if (x < y) 
--(...)
```

### 3.
```hs
-- arith3broken.hs 
module Arith3Broken where 
  main :: IO () 
  Main = do 
    print 1 + 2 
    putStrLn 10 
    print (negate -1) 
    print ((+) 0 blah) 
    where 
      blah = negate 1
```
---
```hs
module Arith3Broken where 
  main :: IO () 
  main = do 
    print $ 1 + 2 
    putStrLn "10" 
    print (negate (-1)) 
    print ((+) 0 blah) 
    where 
      blah = negate 1
```

## Type-Kwon-Do

The focus here is on manipulating terms in order to get the types to fit.

We provide the types and bottomed out (declared as undefined) terms. Bottom and undefined will be explained in more detail later. The contents of the terms are irrelevant here. You’ll use only the declarations provided and what the Prelude provides by default unless otherwise specified. Your goal is to make the ???’d declaration pass the typechecker by modifying it alone.

### 1.
```hs
f :: Int -> String 
f = undefined 

g :: String -> Char 
g = undefined 

h :: Int -> Char 
h = ???
```
---
```hs 
h x = g $ f x
```

### 2.
```hs
data A 
data B 
data C 

q :: A -> B 
q = undefined 

w :: B -> C 
w = undefined 

e :: A -> C 
e = ???
```
---
```hs 
e a = w $ q a
```
### 3.
```hs
data X
data Y 
data Z

xz :: X -> Z 
xz = undefined 

yz :: Y -> Z 
yz = undefined 

xform :: (X, Y) -> (Z, Z) 
xform = ???
```
---
```hs
xform (x, y) = (xz x, yz y)
```

### 4.
```hs
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge = ???
```
---
```hs
munge xToy yTo_wz x = fst $ yTo_wz $ xToy x
```
