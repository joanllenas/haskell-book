# Exercieses




## Eq instances (pg.181)

Write the `Eq` instances for the datatypes provided.

### 1.
```hs
data TisAnInteger =
  TisAn Integer
```
---
```hs
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'
```

### 2. 
```hs
data TwoIntegers = 
  Two Integer Integer
```
---
```hs
instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = x == x' && y == y'
```

### 3. 
```hs
data StringOrInt 
  = TisAnInt Int
  | TisAString String
```
---
```hs
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _ = False
```

### 4. 
```hs
data Pair a =
  Pair a a
```
---
```hs
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

{-- 
With the `-Wall` flag set I get these warnings:
$ (Pair 1 2) == (Pair 1 2)
<interactive>:40:1: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘Integer’
        (Eq a0) arising from a use of ‘==’ at <interactive>:40:1-24
        (Num a0) arising from the literal ‘1’ at <interactive>:40:7
    • In the expression: (Pair 1 2) == (Pair 1 2)
      In an equation for ‘it’: it = (Pair 1 2) == (Pair 1 2)
True
-}
```

### 5. 
```hs
data Tuple a b =
  Tuple a b
```
---
```hs
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'
-- Same warnings when using numbers
```

### 6. 
```hs
data Which a 
  = ThisOne a 
  | ThatOne a
```
---
```hs
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False
-- Same warning when using numbers
```

### 7.
```hs
data EitherOr a b 
  = Hello a
  | Goodbye b
```
---
```hs
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _ = False
{-- 
Why?
$ Hello 'a' == Hello 'b'

<interactive>:64:1: warning: [-Wtype-defaults]
    • Defaulting the following constraint to type ‘()’
        Eq b0 arising from a use of ‘==’
    • In the expression: Hello 'a' == Hello 'b'
      In an equation for ‘it’: it = Hello 'a' == Hello 'b'

<interactive>:64:1: warning: [-Wtype-defaults]
    • Defaulting the following constraint to type ‘()’
        Eq b0 arising from a use of ‘==’
    • In the expression: Hello 'a' == Hello 'b'
      In an equation for ‘it’: it = Hello 'a' == Hello 'b'
False
-}
```



## Exercise: Tuple experiment (pg.183)

Look at the types given for `quotRem` and `divMod`. What do you think those functions do? Test your hypotheses by playing with them in the REPL. We’ve given you a sample to start with below:

```Prelude> ones x = snd (divMod x 10)```

---

```
λ> :t quotRem
quotRem :: Integral a => a -> a -> (a, a)
λ> :t divMod
divMod :: Integral a => a -> a -> (a, a)
```

- quotRem
```hs
myQuotRem :: Integral a => a -> a -> (a, a)
myQuotRem x y = ( x `quot` y, x `rem` y )
```

- divMod
```hs
myDivMod :: Integral a => a -> a -> (a, a)
myDivMod x y = ( x `div` y, x `mod` y )
```




## Put on your thinking cap (pg.185)

Why didn’t we need to make the type of the function we wrote require both type classes? Why didn’t we have to do this:

```f :: (Num a, Fractional a) => a -> a -> a```

Consider what it means for something to be a subset of a larger set
of objects.
---
Because `Fractional` is a `Num` already, being explicit about it is not needed.




## Exercises: Will they work? (pg.195)

Next, take a look at the following code examples, and try to decide if they will work, what result they will return if they do, and why or why not.

### 1.
```hs
max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
```
---
- Will work, `length` returns `Int`, which is an `Ord`.
- Returns `5::Int`.

### 2. 
```hs
compare (3 * 4) (3 * 5) 
```
---
- Will work, (*) returns `Num`, which is an `Ord`, not because it has an instance defined, but because when inferred to one of its subclasses `Integer`, `Int`, `Float`, `Double` or `Word`, they declare the `Ord` instance. 
- Returns `LT`.

### 3. 
```hs
compare 
  "Julie" 
  True
```
---
- Won't work. Although `"Julie"` and `True` are both `Ord` instances, the `compare` function expects both params to be of the same type.

### 4. 
```hs
(5 + 3) > (3 + 6)
```
---
- Will work.
- Returns `False`





# Chapter Exercises (pg.208)

## Multiple Choice

### 1. 
The `Eq` class:
- a) includes all types in Haskell. 
- b) is the same as the `Ord` class.
- c) makes equality tests possible.
- d) only includes numeric types. 
---
Is `c)`

### 2. 
The type class `Ord`:
- a) allows any two values to be compared. 
- b) is a subclass of `Eq`.
- c) is a superclass of `Eq`.
- d) has no instance for `Bool`.
---
Is `a)` and `b)`.
- Is a subclass of `Eq` because you need `Eq` to be able to sort/compare.

### 3. 
Suppose the type class `Ord` has an operator `>`. What is the type of `>`?
- a) `Ord a => a -> a -> Bool `
- b) `Ord a => Int -> Bool`
- c) `Ord a => a -> Char`
- d) `Ord a => Char -> [Char]`
---
Is `a)`

### 4. 
In `x = divMod 16 12`:
- a) the type of `x` is `Integer`.
- b) the value of `x` is undecidable.
- c) the type of `x` is a tuple. 
- d) `x` isequalto `12 / 16`.
---
Is `c)`. It's an `(Integral, Integral)` tuple.

### 5. 
The type class `Integral` includes:
- a) `Int` and `Integer` numbers.
- b) integral, real, and fractional numbers.
- c) Schrodinger’s cat.
- d) only positive numbers.
---
It's `a)`.




## Does it type check? (pg.209)

For this section of exercises, you’ll be practicing looking for type and type class errors.

### 1.
Does the following code type check? If not, why not?
```hs
data Person = Person Bool
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
```
---
It doesn't type check because the `Person` data type does not derive `Show`.

### 2.
Does the following code type check? If not, why not?
```hs
data Mood 
  = Blah
  | Woot deriving Show
settleDown x 
  = if x == Woot then Blah else x
```
---
It doesn't type check because `Mood` does not derive `Eq`.

### 3.
If you were able to get `settleDown` to type check:
- a) What values are acceptable inputs to that function?
- b) What will happen if you try to run `settleDown 9`? Why?
- c) What will happen if you try to run `Blah > Woot`? Why?
---
- a) Acceptable inputs are `Blah` and `Woot`.
- b) `settleDown 9` won't type check because it's not part of the `Mood` data type.
- c) `Blah > Woot` won't type check because `Mood` is not an `Ord`.

### 4.
Does the following code type check? If not, why not?
```hs
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
```
---

It type checks. 
- `s1` is a partially applied product type: `Object -> Sentence`.
- `s2` is a `Sentence`.




## Given a datatype declaration, what can we do? (pg.211)

Given the following datatype definitions:

```hs
data Rocks =
  Rocks String deriving (Eq, Show)
data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)
```

Which of the following will type check? For the ones that don’t type check, why don’t they?

### 1. 
```hs
phew = Papu "chases" True
```
---
Won't type check. Missing the data constructors.
Fixed version:
```hs
phew = Papu (Rocks "chases") (Yeah True)
```

### 2. 
```hs
truth = Papu (Rocks "chomskydoz") (Yeah True)
```
---
Type checks just fine.

### 3. 
```hs
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
```
---
Type checks just fine.

### 4. 
```hs
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
```
---
Won't type check. `Papu` doesn't derive `Ord`, which is required by `>`.




## Match the types (pg.212)

We’re going to give you two types and their implementations. Then we’re going to ask you if you can substitute the second type for the first. You can test this by typing the first declaration and its type into a file and editing in the new one, loading to see if it fails. Don’t guess—test all your answers!

### 1. 
For the following definition:
- a) 
```hs
i :: Num a => a
i = 1
```
- b) Try replacing the type signature with the following:
```hs
i :: a
```

After you’ve formulated your own answer, test that answer. Use GHCi to check what type GHC infers for the definitions we provide without a type assigned. For this exercise, you’d type in:
```
Prelude> i = 1
Prelude> :t i
-- Result intentionally elided
```
---
~~Yes, `i :: Num a => a` can be substituted for `i :: a`.~~
No, `i :: a` cannot be implemented, is too generic.

### 2. 
- a)
```hs
f :: Float
f = 1.0
```
- b) 
```hs
f :: Num a => a
```
---
~~Yes, `f :: Float; f = 1.0` can be substituted for `f :: Num a => a; f = 1.0`~~
No, `Num` is too generic, `Fractional` is the most generic type that can be used for `1.0`.

### 3.
- a) 
```hs
f :: Float
f = 1.0
```
- b) 
```hs
f :: Fractional a => a
```
---
Yes, `f :: Float; f = 1.0` can be changed for `f :: Fractional a => a; f = 1.0` because `1.0` most generic type is `Fractional`.

### 4.
- a) 
```hs
f :: Float 
f = 1.0
```
- b) 
```hs
f :: RealFrac a => a
```
---
Yes, `f :: Float; f = 1.0` can be changed for `f :: RealFrac a => a; f = 1.0`.  `RealFrac` is compatible with `1.0`.

### 5.
- a)
```hs
freud :: a -> a
freud x = x
```
- b) 
```hs
freud ::  Ord a => a -> a
```
---
Yes, it's an identity function specialized for `Ord` instances.

### 6.
- a)
```hs
freud' :: a -> a 
freud' x = x
```
- b) 
```hs
freud' :: Int -> Int
```
---
Yes, it's an identity function specialized for `Int` instances.

### 7.
- a)
```hs
myX = 1 :: Int
sigmund :: Int -> Int 
sigmund x = myX
```
- b) 
```hs
sigmund :: a -> a
```
---
No, `sigmund :: Int -> Int ` can't be changed for `sigmund :: a -> a`. `myX` is an `Int`, so `a -> a` is not compatible with that restriction.

### 8.
- a)
```hs
myX = 1::Int
sigmund' :: Int -> Int 
sigmund' x = myX
```
- b) 
```hs
sigmund' :: Num a => a -> a
```
---
No, since `myX` is `Int`, we can't downgrade the function type to a more polymorphic type.

### 9.
- a) You’ll need to `import sort from Data.List`:
```hs
jung :: Ord a => [a] -> a
jung xs = head (sort xs)
```
- b)
```hs
jung :: [Int] -> Int
````
---
Yes, `jung :: Ord a => [a] -> a` can be changed for `jung :: [Int] -> Int` because `Int` has `Ord`.

### 10.
- a)
```hs
young :: [Char] -> Char
young xs = head (sort xs)
```
- b)
```hs
young :: Ord a => [a] -> a
```
---
Yes, as long as `a` is `Ord`, it can be any type.

### 11.
- a) 
```hs
mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
signifier xs = head (mySort xs) 
```
- b) 
```hs
signifier :: Ord a => [a] -> a
```
---
No, because `mySort` expects a `[Char]` and we are trying to give it any `Ord` instance.




## Type-Kwon-Do Two: Electric typealoo (pg.214)

The idea with these exercises is that you’ll de- rive the implementations from the type information.

### 1.
```hs
chk :: Eq b => (a -> b) -> a -> b -> Bool 
```
---
```hs
chk _ _ _ = True
```
The only thing that can be done with `b` that returns a Bool is comparing with itself, hence `True`, but it could have been implemented as `chk _ _ _ = False` too.

### 2.
Hint: use some arithmetic operation to combine values of type `b`.
```hs
arith :: Num b => (a -> b) -> Integer -> a -> b
```
---
```hs
arith aTob _ a = aTob a
```
Can be used like:
```
$ arith (+1) 999 3
4
```
