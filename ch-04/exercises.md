# Exercises

## Mood Swing

Given the following datatype, answer the following questions:

```hs
data Mood = Blah | Woot dderiving Show
```

### 1.
What is the type constructor, or name of this type?

---

`Mood`

### 2.
If the function requires a `Mood` value, what are the values you could possibly use there?

---

`Blah` or `Woot`

### 3.
We are trying to write a function `changeMood` to change Chris’s mood instantaneously. It should act like not in that, given one value, it returns the other value of the same type. So far, we’ve written a type signature `changeMood :: Mood -> Woot`. What’s wrong with that?

---

Shoud be `changeMood :: Mood -> Mood` because types use type constructors no data constructors.

### 4.
Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function: 

```hs
changeMood Mood = Woot
changeMood _ = Blah
```

---

Pattern matching is done over data constructors, not type constructors.

```hs
changeMood Blah = Woot
changeMood _ = Blah
```
### 5.

```hs
module Moods where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah
```

## Find the Mistakes

The following lines of code may have mistakes — some of them won’t compile! Fix them.

### 1.
`not True && True`

---

Ok.

### 2.
`not (x = 6)`

---

1. `x` is not in scope, so wrap it in a function.
2. The equal sign is assigning instead of comparing.

```hs
fn :: Int -> Bool
fn x = not (x == 6)
```

### 3.
`(1 * 2) > 5`

---

Ok.

### 4.
`[Merry] > [Happy]`

---

Merry and happy should be quoted.

`["Merry"] > ["Happy"]`

### 5.
`[1, 2, 3] ++ "look at me!"`

---

Types don't match, `Int`s should be single quoted so they are `Char`s:

`['1', '2', '3'] ++ "look at me!"`

## 4.9 Chapter Exercises

```hs
awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"] 
allAwesome = [awesome, alsoAwesome]
```

> `length` is a function that takes a list and returns a result that tells how many items are in the list.

### 1.
Given the definition of `length` above, what would the type signature be? How many arguments, of what type does it take? What is the type of the result it evaluates to? 

---

```hs
length :: [a] -> Int
```

### 2. 
What are the results of the following expressions? 
- a) `length [1, 2, 3, 4, 5]`
- b) `length [(1, 2), (2, 3), (3, 4)]`
- c) `length allAwesome`
- d) `length (concat allAwesome)`

---

- a) `5`
- b) `3`
- c) `2`
- d) `5`

### 3. 
Given what we know about numeric types and the type signature of length, look at these two expressions. One works and one returns an error. Determine which will return an error and why. 

> (n.b., you will find `Foldable t => t a` representing `[a]`, as with `concat` in the previous chapter. Again, consider `Foldable t` to represent a list here, even though list is only one of the possible types.)

`Prelude> 6 / 3`

and 

`Prelude> 6 / length [1, 2, 3]`

---

`6 / length [1, 2, 3]` will error because `/` expects two `Fractional` arguments and `length` returns an `Int`.

### 4. 
How can you fix the broken code from the preceding exercise using a different division function/operator? 

---

```hs
6 `div` length [1, 2, 3]
```

### 5. 
What is the type of the expression `2 + 3 == 5`? What would we expect as a result? 

---

### 6. 
What is the type and expected result value of the following:

```
Prelude> let x = 5
Prelude> x + 3 == 5
```

---

Type `Bool`, value `False`.

### 7. 
Below are some bits of code. Which will work? Why or why not? If they will work, what value would these reduce to?


1. `Prelude> length allAwesome == 2`
2. `Prelude> length [1, 'a', 3, 'b']`
3. `Prelude> length allAwesome + length awesome`
4. `Prelude> (8 == 8) && ('b' < 'a')`
5. `Prelude> (8 == 8) && 9`

---

1. Won't work. Trying to compare a list with a number.
2. Won't work. All list items must have the same type.
3. Will work. Result: `5`.
4. Will work. Result: `False`
5. Won't work. `&&` expects `Bool` arguments, and `9` is not.

### 8. 
Write a function that tells you whether or not a given String (or list) is a palindrome. Here you’ll want to use a function called ’reverse,’ a predefined function that does just what it sounds like. 

```
reverse :: [a] -> [a] 
reverse "blah" 
"halb"
```

---

* With `reverse`:

```hs
isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome xs = reverse a === a
```

* Without `reverse`:

```hs
isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome xs =
  if 
    len > 1 
  then 
    compareTails xs 0 (len-1) True
  else 
    False
  where
    len = length xs
    compareTails :: (Eq a) => [a] -> Int -> Int -> Bool -> Bool
    compareTails xs2 indexL indexR acc =
      let
        tailLettersMatch = (xs2 !! indexL) == (xs2 !! indexR)
        acc2 = acc && tailLettersMatch
      in
        if 
          ((indexL + 1) == (indexR -1) || (indexL + 1) > indexR )
        then 
          acc2
        else
          compareTails xs2 (indexL+1) (indexR-1) acc2
```

### 9. 
Write a function to return the absolute value of a number using if-then-else 

---

```hs
myAbs :: Integer -> Integer 
myAbs x = if x < 0 then negate x else x
```

### 10. 
Fill in the definition of the following function, using `fst` and `snd`:

---

```hs
f :: (a, b) -> (c, d) -> ((b, d), (a, c)) 
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))
```

## Correcting Syntax

In the following examples, you’ll be shown syntactically incorrect code. Type it in and try to correct it in your text editor.

### 1.
Here, we want a function that adds 1 to the length of a string argument and returns that result. 

```hs
x = (+) 
F xs = w 'x' 1 
  where w = length xs 
```
---
- `F` should be `f`
- `'x'` should be `` `x` ``

```hs
x = (+) 
f xs = w `x` 1 
  where w = length xs 
```

### 2.

This is supposed to be the identity function, `id`. 

```hs
\ X = x 
```
---
- `X` should be `x`
- `=` should be `->`

```hs
\x -> x 
```

### 3.
When fixed, this function will return `1` from the value `[1, 2, 3]`. Hint: you may need to refer back to the section about variables conventions in `“Hello Haskell”` to refresh your memory of this notation. 

```hs
\ x : xs -> x
```
---
- `x : xs` should be parenthesized.

```hs
\ (x : xs) -> x
```

### 4.
When fixed, this function will return `1` from the value `(1, 2)`.

```hs
f (a b) = A 
```
---
- `A` should be `a`.
- `(a b)` should be `(a, b)`.

```hs
f (a, b) = a
```

## Match the function names to their types 

### 1.
Which of the following types is the type of `show`? 
- a) `show a => a -> String`
- b) `Show a -> a -> String`
- c) `Show a => a -> String`
---
It's `c`.

### 2. 
Which of the following types is the type of `(==)`? 
- a) `a -> a -> Bool`
- b) `Eq a => a -> a -> Bool`
- c) `Eq a -> a -> a -> Bool`
- d) `Eq a => A -> Bool`
---
It's `b`.

### 3.
Which of the following types is the type of `fst`? 
- a) `(a, b) -> a`
- b) `b -> a`
- c) `(a, b) -> b`
---
It's `a`.

4. Which of the following types is the type of `(+)`?

- a) `(+) :: Num a -> a -> a -> Bool`
- b) `(+) :: Num a => a -> a -> Bool`
- c) `(+) :: num a => a -> a -> a`
- d) `(+) :: Num a => a -> a -> a`
- e) `(+) :: a -> a -> a`
---
It's `d`.