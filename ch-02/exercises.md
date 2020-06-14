# Exercises

## Comprehension Check

### #1

Given the following lines of code as they might appear in a source file:

```
half x = x / 2 
square x = x * x
```

How would you change them to use them directly in the REPL? 

---

```hs
let half x = x / 2

let sqaure x = x * 2
```

### #2

Write one function that can accept one argument and work for all the following expressions. Be sure to name the function. 

```
3.14 * (5 * 5) 
3.14 * (10 * 10) 
3.14 * (2 * 2) 
3.14 * (4 * 4)
```

---

```hs
fn x = 3.14 * (x * x)
```

### #3

There is a value in Prelude called pi. Rewrite your function to use pi instead of 3.14.

---

```hs
fn x = pi * (x * x)
```

## Parentheses and Association

Below are some pairs of functions that are alike except for parenthesization. Read them carefully and decide if the parentheses change the results of the function.

### #1 
a) `8 + 7 * 9` 

b) `(8 + 7) * 9` 

> Changes the result.

### #2 
a) `perimeter x y = (x * 2) + (y * 2)`

b) `perimeter x y = x * 2 + y * 2`

> Doesn't change the result.

### #3
a) `f x = x / 2 + 9`

b) `f x = x / (2 + 9)`

> Changes the result.

## Heal the sick

The following code samples are broken and won’t compile. The first two are as you might enter into the REPL; the third is from a source file. Find the mistakes and fix them so that they will.

### #1

```
let area x = 3. 14 * (x * x)
```
---

```hs
let area x = 3.14 * (x * x)
```

### #2

```
let double x = b * 2
```
---

```hs
let double x = x * 2
```

### #3

```
x = 7 
 y = 10 
f = x + y
```
---

```hs
x = 7 
y = 10 
f = x + y
```

## A Head of Code

Now for some exercises. First, determine in your head what the following expressions will return, then validate in the REPL:

### #1

`let x = 5 in x`

> 5

### #2

`let x = 5 in x * x`

> 25

### #3

`let x = 5; y = 6 in x * y`

> 30

### #4

`let x = 3; y = 1000 in x + 3`

> 6

## A Head of Code (cont)

Rewrite `let` expressions in functions using `where` declarations (in a module file).

### #1

`let x = 3; y = 1000 in x * 3 + y `

---

```hs
fn1 = x * 3 + y
  where
    x = 3
    y = 1000
```

### #2 

`let y = 10; x = 10 * 5 + y in x * 5 `

---

```hs
fn2 = x * 5
  where
    y = 10
    x = 10 * 5 + y
```

### #3 

`let x = 7; y = negate x; z = y * 10 in z / x + y`

---

```hs
fn3 = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10
```

# Chapter Exercises

## Parenthesization

How can we parenthesize the following expressions more explicitly without changing their results?

### #1

`2 + 2 * 3 - 1`

---

```hs
2 + (2 * 3) - 1
```

### #2

`(^) 10 $ 1 + 1`

---

```hs
(^) 10 (1 + 1)
```

### #3

`2 ^ 2 * 4 ^ 5 + 1`

---

```hs
(2 ^ 2) * (4 ^ 5) + 1
```

## Equivalent expressions 

Which of the following pairs of expressions will return the same result when evaluated?

### #1

1. `1 + 1`
2. `2`

> Both return the same

### #2

1. `10 ^ 2`
2. `10 + 9 * 10`

> Both return the same

### #3 

1. `400 - 37`
2. `(-) 37 400`

> Return different things

### #4 

1. ```100 `div` 3``` 
2. `100 / 3`

> Return different things

### #5 

1. `2 * 5 + 18`
2. `2 * (5 + 18)`

> Return different things

## More fun with functions

Look at this code and rewrite it such that it could be evaluated in the REPL.

```
z = 7
x = y ^ 2
waxOn = x * 5
y = z + 8
```
---

`λ> z = 7`

`λ> y = z + 8`

`λ> x = y ^ 2`

`λ> waxOn = x * 5`

## More fun with functions (cont)

### #1

Now you have a value called waxOn in your REPL. What do you think will happen if you enter:

- Q: `10 + waxOn`
- A: `1135`
---
- Q: `(+10) waxOn`
- A: `1135`
---
- Q: `(-) 15 waxOn`
- A: `-1110`
---
- Q: `(-) waxOn 15`
- A: `1110`

### #2

While your REPL has waxOn in session, re-enter the triple function at the prompt: 

```hs
let triple x = x * 3
```

### #3

Now, what will happen if we enter this at our GHCi prompt.

```hs
triple waxOn
```

---

```
λ> triple waxOn
3375
```

### #4

Rewrite `waxOn` as an expression with a where clause in your source file. Load it into your REPL and make sure it still works as expected!

```hs
waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^2
```
### #5

Now to the same source file where you have waxOn, add the triple function, then enter triple waxOn again at the REPL prompt.

```hs
triple x = x * 3
```

### #6

Now, without changing what you’ve done so far in that file, add a new function called waxOff that looks like this: 

```waxOff x = triple x```

### #7

Load the source file into your REPL and enter waxOff waxOn at the prompt.

```
λ> waxOff waxOn
3375
```
