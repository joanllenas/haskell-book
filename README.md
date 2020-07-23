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

## Forms (NF, WHNF)

- **NF**: An expression is in **Normal Form** when It is fully evaluated.
- **WHNF**: An expression is in **Weak Head Normal Form** when:
  - It has been evaluated to the point of arriving at a data constructor.
  - It has been evaluated to the point of arriving at a lambda awaiting an argument.
  - If no further inputs are possible, then it is still in **WHNF** but also in **NF**.


## newtype vs type vs data

- **`newtype` vs `type`**: 
  - A `newtype` is similar to a `type` synonym in that the representations of the named type and the type it contains are identical and any distinction between them is stripped away at compile time. So, a `String` really is a `[Char]`, and `Goats` in `newtype Goats = Goats Int deriving (Eq, Show)`  is really an `Int`.
  - You can define type class instances for a `newtype` that differs from the instances for its underlying type. You can’t do that for `type` synonyms. (pg.406)

## Smart constructor

```hs
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
mkPerson :: Name -> Age -> Maybe Person mkPerson name age -- Smart constructor
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing
```

# Vocabulary

- **Constrained Polymorphism**: `(/) :: Fractional a => a -> a -> a`. Here `Fractional a => a` constrains the type variable `a` to the type `Fractional`, so it is mandatory that `a` is a `Fractional`.

- **Parametric Polymorphism**: `id a = a`. Here `a` can be any type, its type has no constrains.

- **catamorphism**: a.k.a Folds. You’re familiar with the root “morphism” from polymorphism. “Cata-” means “down” or “against,” as in “catacombs.” Catamorphisms are a means of deconstructing data. If the spine of a list is the structure of a list, then a fold is what can reduce that structure.

- **arity**: The number of arguments that a constructor (or function) takes.
  - **nullary**: zero arguments. i.e. `True`, `Nothing`.
  - **unary**: one argument. i.e. `Just a`
  - **products**: two or more arguments. i.e. `Time Int Int Int`

- **cardinality**: The number of different combinations of a type. i.e. `Bool` = 2 (`True` and `False`), `Int8` = minBound::Int8 + maxBound::Int8 + 1 = 128 + 127 + 1 = 256.

# Algebraic properties

All the existing algebraic rules for products and sums apply in type system:

- **distributive**: The distributive property can be generalized as follows: `a * (b + c) -> (a * b) + (a * c)`, and this is true of Haskell’s types as well! Product types distribute over sum types.

