# Folds

Folding happens in two stages, 
- **Traversal**: is the stage in which the fold recurses over the spine 
- **Folding**: refers to the evaluation or reduction of the folding function applied to the values.

> Given this two-stage process and non-strict evaluation, if f doesn’t evaluate its second argument (the rest of the fold), no more of the spine will be forced. One of the consequences of this is that foldr can avoid evaluating not only some or all of the values in the list, but some or all of the list’s spine, as well! For this reason, foldr can be used with lists that are potentially infinite.

## foldr

### Implementation

```hs
-- `a` is the next value from the list
-- `b` is the accumulator
foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
```

### Example

```
-- foldr :: (a -> b -> b) -> b -> a -> b
$ foldr (-) 100 [1,2,3]
-98
```

---

```(-) 1 ((-) 2 ((-) 3 100))```
```(-) 1 ((-) 2 (-97))```
```(-) 1 99```
```-98```

**Or:**

```1 - (2 - (3 - 100))```

```
3 - 100 = -97
2 - (-97) = 99
1 - 99 = -98
```

### Another Example

```
foldr f z [1, 2, 3]
1 `f` (foldr f z [2, 3])
1 `f` (2 `f` (foldr f z [3]))
1 `f` (2 `f` (3 `f` (foldr f z []))) 
1 `f` (2 `f` (3 `f` z))
```

_Parentheses are real_. In the above, the 3 `f` z pairing gets evaluated first, because it’s in the innermost parentheses

### foldr visualization trick

```hs
xs = map show [1..5]
y = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs
```

```
Prelude> y
"(1+(2+(3+(4+(5+0)))))"
```

### foldr infinite

```hs
myAny :: (a -> Bool) -> [a] -> Bool 
myAny f xs = foldr (\x acc -> f x || acc) False xs

gt10 x = x > 10
myAny gt10 False [1..]


{-
(gt10 12 || (gt10 11 || (gt10 10 || (gt10 9 || (gt10 8 || (gt10 7 || (gt10 6 || (gt10 5 || (gt10 4 || (gt10 3 || (gt10 2 || (gt10 1 || False)))))))))))
             ^           ^           ^          ^          ^          ^          ^          ^          ^          ^          ^
 ^ Not       True        False       False      False      False      False      False      False      False      False      False
 ^ reached
-}
```


## foldl

### Implementation

```hs
foldl :: (b -> a -> b) -> b -> [a] -> b 
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
```

### Example

```hs
foldl (+) 0 [1,2,3] 
-- (+) ((+) ((+) 0 1) 2) 3
-- ((0 + 1) + 2) + 3
--   ^   ^    ^    ^
--   0   1    3    6
-- 6
```


### foldl visualization trick

```hs
conc = concat
f x y = conc ["(",x,"+",y,")"]
```

```
Prelude> foldl f "0" (map show [1..5])
"(((((0+1)+2)+3)+4)+5)"
```

We can see from this that `foldl` begins its reduction process by adding the acc (accumulator) value to the head of the list, whereas `foldr` adds it to the final element of the list, first.

### foldl infinite

**`foldl` is generally inappropriate with lists that are or could be infinite**. 

Also, the combination of the forced spine evaluation with non-strictness means that **it is also usually inappropriate even for long lists**, as the forced evaluation of the spine affects performance negatively. Because foldl must evaluate its whole spine before it starts evaluating values in each cell, it accumulates a pile of unevaluated values as it traverses the spine.

In most cases, when you need a left fold, you should use `foldl'`. This function, called “fold-l-prime,” works the same way, except it is strict. In other words, it forces evaluation of the values inside the cons cells as it traverses the spine, rather than accumulating unevaluated expressions for each element of a list. The strict evaluation here means it has less negative effect on performance over long lists.


## Contrasting both folds

```
foldr (^) 2 [1..3]
(1 ^ (2 ^ (3 ^ 2)))
(1 ^ (2 ^ 9)) 
1 ^ 512
1
```

```
foldl (^) 2 [1..3] 
((2 ^ 1) ^ 2) ^ 3 
(2 ^ 2) ^ 3
4 ^ 3
64
```

Another:

```
foldr (:) [] [1..3]
(1 : (2 : (3 : [])))
[3]
[2,3]
[1,2,3]
```

```
foldl (flip (:)) [] [1..3]
((([] : 1) : 2) : 3)
[1]
[2,1]
[3,2,1]
```

