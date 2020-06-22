module EqInstances where

--  Write the `Eq` instances for the datatypes provided.

-- 1.
data TisAnInteger =
  TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'

-- 2.
data TwoIntegers = 
  Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = x == x' && y == y'

-- 3.
data StringOrInt 
  = TisAnInt Int
  | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _ = False

-- 4.
data Pair a =
  Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

{-- 
With the `-Wall` flag set I get these warnings:
```
$ (Pair 1 2) == (Pair 1 2)
<interactive>:40:1: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘Integer’
        (Eq a0) arising from a use of ‘==’ at <interactive>:40:1-24
        (Num a0) arising from the literal ‘1’ at <interactive>:40:7
    • In the expression: (Pair 1 2) == (Pair 1 2)
      In an equation for ‘it’: it = (Pair 1 2) == (Pair 1 2)
True
```
-}

-- 5.
data Tuple a b =
  Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'
-- Same warnings when using numbers

-- 6.
data Which a 
  = ThisOne a 
  | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False
-- Same warning when using numbers

-- 7.
data EitherOr a b 
  = Hello a
  | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _ = False
{-- 
Why?
```
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
```
-}