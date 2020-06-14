# Exercises

## Scope

### 1.

These lines of code are from a REPL session. Is `𝑦` in scope for `𝑧`? 

```
Prelude> let x = 5 
Prelude> let y = 7 
Prelude> let z = x * y
```

---

> Yes

### 2.

These lines of code are from a REPL session. Is `ℎ` in scope for function `𝑔`? Go with your gut here. 

```
Prelude> let f = 3 
Prelude> let g = 6 * f + h
```

---

> No, `h` is not in scope.

### 3.

This code sample is from a source file. Is everything we need to execute `area` in scope? 

```hs
area d = pi * (r * r) 
r = d / 2
```

---

> No, `d` is no in scope for `r`.

### 4.

This code is also from a source file. Now are `𝑟` and `𝑑` in scope for `area`? 

```hs
area d = 
  pi * (r * r) 
  where r = d / 2
```

---

> Yes

## Syntax Errors

Read the syntax of the following functions and decide whether it will compile.

### 1.
`++ [1, 2, 3] [4, 5, 6]` 

---

It won't compile because `++` shoud be `(++)`.

### 2.

`'<3' ++ ' Haskell'`

---

It won't compile because strings shoud use `"` instead of `'`.

### 3.

`concat ["<3", " Haskell"]`

---
It will compile just fine.

## Reading Syntax

### 1.

For the following lines of code, read the syntax carefully and decide if they are written correctly. Correct as many as you can.

#### a)

`concat [[1, 2, 3], [4, 5, 6]]`

---

Ok.

#### b) 

`++ [1, 2, 3] [4, 5, 6]`

---

Should be: `(++) [1, 2, 3] [4, 5, 6]`

#### c) 

`(++) "hello" " world" `

---

Ok.

#### d) 

`["hello" ++ " world]`

---

Should be: `["hello" ++ " world"]`

#### e) 

`4 !! "hello"`

---

Shoud be `"hello" !! 4`

#### f) 

`(!!) "hello" 4`

---

Ok.

#### g) 

`take "4 lovely"` 

---

Shoud be: `take 4 "lovely"`

#### h) 

`take 3 "awesome"`

---

Ok.

### 2.

Next we have two sets: the first set is lines of code and the other is a set of results. Read the code and figure out which results came from which lines of code.

#### a) 
`concat [[1 * 6], [2 * 6], [3 * 6]] `

#### b) 
`"rain" ++ drop 2 "elbow"`

#### c) 
`10 * head [1, 2, 3]`

#### d) 
`(take 3 "Julie") ++ (tail "yes")`

#### e) 
```
concat [tail [1, 2, 3], 
        tail [4, 5, 6], 
        tail [7, 8, 9]]
```

Match with:

#### A) `"Jules" `
#### B) `[2,3,5,6,8,9] `
#### C) `"rainbow" `
#### D) `[6,12,18] `
#### E) `10`

---

- **a)** with **D)**
- **b)** with **C)**
- **c)** with **E)**
- **d)** with **A)**
- **e)** with **B)**


## Building Functions

### 1.

Given the list-manipulation functions mentioned in this chapter, write functions that take the following inputs and return the expected outputs. Do them directly in your REPL and use the `take` and `drop` functions you’ve already seen.

#### a)
-- Given 

`"Curry is awesome" `

-- Return 

`"Curry is awesome!"`

---

```hs
"Curry is awesome" ++ "!"
```

#### b)
-- Given 

`"Curry is awesome!"` 

-- Return 
`"y"`

---

```hs
"Curry is awesome!" !! 4
```

#### c) 

-- Given 

`"Curry is awesome!" `

-- Return 

`"awesome!"`

---

```hs
drop 9 "Curry is awesome!"
```

### 2.

Now take each of the above and rewrite it in a source file as a general function that could take different string inputs as arguments but retain the same behavior. Use a variable as the argument to your (named) functions.

All start with:

```hs
module Exercises where
```

#### a)

```hs
fna :: String -> String
fna x = "Curry is awesome" ++ x
```

#### b)

```hs
fnb :: Int -> Char
fnb x = "Curry is awesome" !! x
```

#### c) 

```hs
fnc :: Int -> String
fnc x = drop x "Curry is awesome!"
```

### 3.

```hs
fn :: String -> Char
fn str = str !! 2
```

### 4.

```hs
fn :: Int -> Char
fn x = "Hello World!" !! x
```

### 5.

```hs
rvrs ....
```
