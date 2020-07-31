## Exercises

## Chapter Exercises (pg.472)

### Determine the kinds

### 1.
Given:

`id :: a -> a`

What is the kind of `a`?

---

It's `* -> *` because it is waiting to be applied.

### 2. 

`r :: a -> f a`

What are the kinds of `a` and `f`?

---

`* -> *`

## String processing (pg.473)

Because this is the kind of thing linguists ahem enjoy doing in their spare time.

1. Write a recursive function named `replaceThe` that takes a text/string, breaks it into words, and replaces each instance of `"the"` with `"a"`. It should only replace exactly the word `"the"`. `notThe` is a suggested helper function for accomplishing this:

```hs
notThe :: String -> Maybe String 
notThe = undefined
```

Example GHCi session using the above functions:
```
Prelude> notThe "the"
Nothing
Prelude> notThe "blahtheblah"
Just "blahtheblah"
Prelude> notThe "woot"
Just "woot"
```

replaceThe :: String -> String 
replaceThe = undefined

```
Prelude> replaceThe "the cow loves us"
"a cow loves us"
```

---

I attempted recursive but didn't have much time, so...

```hs
import Data.List as List

notThe :: String -> Maybe String 
notThe "the" = Nothing
notThe s = Just s

the2a :: Maybe String -> String
the2a Nothing = "a"
the2a (Just s) = s

replaceThe :: String -> String 
replaceThe s 
  = List.intercalate " " 
  . map (the2a . notThe)
  $ words s
```

### 2.
Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of `"the"` followed by a vowel-initial word:

```hs
countTheBeforeVowel :: String -> Integer 
countTheBeforeVowel = undefined
```

```
Prelude> countTheBeforeVowel "the cow"
0
Prelude> countTheBeforeVowel "the evil cow"
1
```

---

```hs
string-processing.hs
```



## Validate the word (pg.474)

Use the `Maybe` type to write a function that counts the number of vowels in a string and the number of consonants. If the number of vowels exceeds the number of consonants, the function returns `Nothing`. In many human languages, vowels rarely exceed the number of consonants, so when they do, it may indicate the input isn’t a word (that is, a valid input to your dataset):

```hs
newtype Word' =
  Word' String 
  deriving (Eq, Show)

vowels = "aeiou"
mkWord :: String -> Maybe Word' 
mkWord = undefined
```

---

```hs
import Data.Bool
import Data.Char

isVowel :: Char -> Bool
isVowel ch = elem ch ['a','e','i','o','u']

newtype Word' =
  Word' String 
  deriving (Eq, Show)

mkWord :: String -> Maybe Word' 
mkWord s
  = (\(v, c) -> bool (Just $ Word' s) Nothing (v > c)) 
  . foldr (\ch (v,c) -> bool (v,c+1) (v+1,c) (isVowel ch)) (0,0) 
  $ filter (\ch -> not $ isSpace ch) s
```



## It’s only natural (pg.475)

You’ll be presented with a datatype to represent the natural numbers. The only values representable with the naturals are whole numbers from zero to infinity. Your task will be to implement functions to convert natural numbers to integers and integers to naturals. The conversion from `Nat` to `Integer` won’t return `Maybe`, because, as you know, `Integer` is a strict superset of `Nat`. Any `Nat` can be represented by an `Integer`, but the same is not true of any `Integer`. Negative numbers are not valid natural numbers:

```hs
-- As natural as any
-- competitive bodybuilder 
data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)
natToInteger :: Nat -> Integer 
natToInteger = undefined
integerToNat :: Integer -> Maybe Nat 
integerToNat = undefined
```

```
Prelude> natToInteger Zero
0
Prelude> natToInteger (Succ Zero)
1
Prelude> natToInteger (Succ (Succ Zero))
2
Prelude> integerToNat 0
Just Zero
Prelude> integerToNat 1
Just (Succ Zero)
Prelude> integerToNat 2
Just (Succ (Succ Zero))
Prelude> integerToNat (-1)
Nothing
```

---

```hs
-- see only-naturals.hs
```


## Small library for Maybe (pg.476)

Write the following functions. This may take some time. 

### 1. 
Simple Boolean checks for `Maybe` values:

```hs
isJust :: Maybe a -> Bool 
isNothing :: Maybe a -> Bool
```

```
Prelude> isJust (Just 1)
True
Prelude> isJust Nothing
False
Prelude> isNothing (Just 1)
False
Prelude> isNothing Nothing
True
```

---

```hs
isJust :: Maybe a -> Bool 
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust
```

### 2. 
The following is the `Maybe` catamorphism. You can turn a `Maybe` value into anything else with this:

```hs
mayybee :: b -> (a -> b) -> Maybe a -> b
```

```
Prelude> mayybee 0 (+1) Nothing
0
Prelude> mayybee 0 (+1) (Just 1)
2
```

---

```hs
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ a2b (Just a) = a2b a
mayybee def _ Nothing = def
```

### 3. 
In case you just want to provide a fallback value. Try writing it in terms of the `maybe` catamorphism:

```hs
fromMaybe :: a -> Maybe a -> a
```

```
Prelude> fromMaybe 0 Nothing
0
Prelude> fromMaybe 0 (Just 1)
1
```

---

```hs
fromMaybe :: a -> Maybe a -> a
fromMaybe a ma = mayybee a id ma
```

### 4. 
Converting between `List` and `Maybe`:

```hs
listToMaybe :: [a] -> Maybe a
maybeToList :: Maybe a -> [a]
```

```
Prelude> listToMaybe [1, 2, 3]
Just 1
Prelude> listToMaybe []
Nothing
Prelude> maybeToList (Just 1)
[1]
Prelude> maybeToList Nothing
[]
```

---

```hs
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

```

### 5. 
For when we want to drop the `Nothing` values from a list: 
```hs
catMaybes :: [Maybe a] -> [a]
```

```
Prelude> catMaybes [Just 1, Nothing, Just 2]
[1, 2]
Prelude> let xs = take 3 $ repeat Nothing
Prelude> catMaybes xs
[]
```

---

```hs
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes xs 
  = map (fromMaybe undefined) 
  $ filter (\x -> isJust x) xs
```

```hs
catMaybes :: [Maybe a] -> [a]
catMaybes []           = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs)  = x : catMaybes xs
```

### 6. 
You’ll see this called sequence later:

```hs
flipMaybe :: [Maybe a] -> Maybe [a]
```

```
Prelude> flipMaybe [Just 1, Just 2, Just 3]
Just [1, 2, 3]
Prelude> flipMaybe [Just 1, Nothing, Just 3]
Nothing
```

---

```hs
justs :: [Maybe a] -> [a] -> Maybe [a]
justs [] acc = Just $ reverse acc
justs (x:xs) acc
  = case x of
    Just x' -> justs xs (x':acc)
    Nothing -> Nothing

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = justs xs []
```



## Small library for Either (pg.477)

Write each of the following functions. If more than one possible unique function exists for the type, use common sense to determine what it should do.

### 1. 
Try to eventually arrive at a solution that uses `foldr`, even if earlier versions don’t use `foldr`:

```hs
lefts' :: [Either a b] -> [a]
```

---

```hs
-- recursive version
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' ((Left x):xs) = x : lefts' xs
lefts' ((Right _):xs) = lefts' xs

-- foldr version
leftAsList :: Either a b -> [a]
leftAsList (Left a) = [a]
leftAsList _ = []

leftsFoldr :: [Either a b] -> [a]
leftsFoldr =  foldr (\x y -> (leftAsList x) ++ y) []
```

### 2. 
Same as the last one. Use `foldr`, eventually:

```hs
rights' :: [Either a b] -> [b]
```

---

```hs
-- recursive version
rights' :: [Either a b] -> [b]
rights' [] = []
rights' ((Left _):xs) = rights' xs
rights' ((Right x):xs) = x : rights' xs

-- foldr version
rightAsList :: Either a b -> [b]
rightAsList (Right x) = [x]
rightAsList _ = []

rightsFoldr :: [Either a b] -> [b]
rightsFoldr =  foldr (\x y -> (rightAsList x) ++ y) []
```

### 3. 
```hs
partitionEithers' :: [Either a b] -> ([a], [b])
```

---

```hs
addIfLeft :: Either a b -> [a] -> [a]
addIfLeft (Left x) xs = x : xs
addIfLeft _ xs = xs

addIfRight :: Either a b -> [b] -> [b]
addIfRight (Right x) xs = x : xs
addIfRight _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' 
  = foldr 
  (\x (as, bs) -> (addIfLeft x as, addIfRight x bs)) 
  ([],[])
```

### 4. 
```hs
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
```

---

```hs
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' b2c (Right b) = Just $ b2c b
eitherMaybe' _ _ = Nothing
```

### 5. 
This is a general catamorphism for `Either` values:

```hs
either' :: (a -> c) -> (b -> c) -> Either a b -> c
```

---

```hs
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' a2c _ (Left a) = a2c a
either' _ b2c (Right b) = b2c b
```

### 6. 
Same as before, but use the `either'` function you just wrote:

```hs
eitherMaybe'' :: (b -> c) -> Either a b-> Maybe c
```

---

```hs
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' b2c x = either' (\_ -> Nothing) (Just . b2c) x
```



## Write your own iterate and unfoldr (pg.480)

### 1. 
Write the function `myIterate` using direct recursion. Compare the behavior with the built-in `iterate` to gauge correctness. Do not look at the source or any examples of iterate, so that you are forced to do this yourself:

```hs
myIterate :: (a -> a) -> a -> [a] 
myIterate f x = x : myIterate f (f x)
```

---

```hs
myIterate :: (a -> a) -> a -> [a] 
myIterate f x = x : myIterate f (f x)
```

### 2. 
Write the function `myUnfoldr` using direct recursion. Compare with the built-in `unfoldr` to check your implementation. Again, don’t look at implementations of unfoldr, so that you figure it out yourself:

```hs
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
myUnfoldr = undefined
```

---

```hs
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
myUnfoldr f b = 
  let
    res = f b
  in
    case res of
      Just (a', b') -> a' : myUnfoldr f b'
      Nothing -> []

-- see unfolds.hs for usage examples.
```


### 3. 
Rewrite `myIterate` into `betterIterate` using `myUnfoldr`. A hint: we use `unfoldr` to produce the same results as `iterate` above. Do this with different functions, and see if you can abstract the structure out.

---

```hs
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x
```



## Finally something other than a list! (pg.481)

Given the `BinaryTree` from the last chapter, complete the following exercises. Here’s that datatype again:

```hs
data BinaryTree a 
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)
```

### 1. 
Write `unfold` for `BinaryTree`:

```hs
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b 
unfold = undefined
```

---

```hs
myUnfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b 
myUnfoldTree f x =
  case f x of
    Just (a,b,a') -> Node (myUnfoldTree f a) b (myUnfoldTree f a')
    Nothing -> Leaf
-- see binary-tree.hs for usage examples
```


### 2. 
Make a tree builder.

Using the `unfold` function you made for `BinaryTree`, write the following function:

```hs
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = undefined
```

---

```hs
treeBuildFn :: Integer -> Maybe (Integer,Integer,Integer)
treeBuildFn a = 
  if a == 0 then Nothing
  else Just (a-1, a-1, a-1)

treeBuild :: Integer -> BinaryTree Integer
treeBuild = myUnfoldTree treeBuildFn

-- for some reason, my implementation goes backwards
```
