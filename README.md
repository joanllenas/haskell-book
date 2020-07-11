# The Haskell book

> Notes and exercises from the "Haskell Programming from first principles" book

# Cheat Sheet

## Declaring Type Classes

```hs
module DayOfWeek where

data DayOfWeek = 
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show

data Date = 
  Date DayOfWeek Int
  deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

instance Eq Date where
  (==) (Date weekDay dayOfMonth) (Date weekDay' dayOfMonth') =
    weekDay == weekDay' && dayOfMonth == dayOfMonth'
```

### Usage

```
$ Date Mon 1 == Date Mon 1
True
```

```
$ Date Tue 1 == Date Mon 1
False
```

## Partial function

> A partial function is one that doesn’t handle all the possible input cases.

```
$ f :: Int -> Bool ; 
$ f 2 = True
```

If you compile or load the above code, everything will work just fine, but if you run it with anything other than 2, you'll get a runtime exception:

```
$ f 3
*** Exception: <interactive>:1:20-29: Non-exhaustive patterns in function f
```

This is also a concern with tpe class instances.

Taking `DayOfWeek` as an example, if we forget to declare the `(==) _ _     = False` case:

```hs
instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
```
If we try to compare two different `Date`s it will crash at runtime.

### :set -Wall

To avoid all that, we can ask the compiler for help by using the `-Wall` flag in our REPL or in our build configuration.

Note the `Pattern match(es) are non-exhaustive` error:

```
$ :set -Wall
$ :l day-of-week.hs 
[1 of 1] Compiling DayOfWeek        ( day-of-week.hs, interpreted )

day-of-week.hs:12:3: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘==’:
        Patterns not matched:
            Mon Tue
            Mon Wed
            Mon Thu
            Mon Fri
            ...
   |
12 |   (==) Mon Mon = True
   |   ^^^^^^^^^^^^^^^^^^^...
Ok, one module loaded.
```

## Writing Type Class instances for fully polymorphic types

`Identity` is a polymorphic data type:

```hs
data Identity a = Identity a
```

Let's try to declare `Identity`'s `Eq` instance:

```hs
instance Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
```

The problem with the above declaration is that the compiler doesn't have enough information to assume that `a` has an `Eq` instance. We need to express that with constrained polymorphism:

```hs
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
```


### Type defaulting in Type classes

> Found in the [Haskell Report](https://www.haskell.org/onlinereport/haskell2010/)

```hs
default Num Integer
default Real Integer
default Enum Integer
default Integral Integer
default Fractional Double
default RealFrac Double
default Floating Double
default RealFloat Double
```

This type defaulting means that, for instance:
```hs
(/) :: Fractional a => a -> a -> a
```
Changes to:
```hs
(/) :: Double -> Double -> Double
```
if you don’t specify the concrete type.




# Vocabulary

- **Constrained Polymorphism**: `(/) :: Fractional a => a -> a -> a`. Here `Fractional a => a` constrains the type variable `a` to the type `Fractional`, so it is mandatory that `a` is a `Fractional`.

- **Parametric Polymorphism**: `id a = a`. Here `a` can be any type, its type has no constrains.

- **catamorphism**: a.k.a Folds. You’re familiar with the root “morphism” from polymorphism. “Cata-” means “down” or “against,” as in “catacombs.” Catamorphisms are a means of deconstructing data. If the spine of a list is the structure of a list, then a fold is what can reduce that structure.
