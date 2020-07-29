# Exercises

## Exercises: Dog types (pg.394)

Given the datatypes defined here:

```hs
data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a 
  = Husky a
  | Mastiff a 
  deriving (Eq, Show)
```

### 1. 
Is `Doggies` a type constructor or a data constructor? 

---

A type constructor.

### 2. 
What is the kind of `Doggies`?

---

`* -> *`

### 3. 
What is the kind of `Doggies String`?

---

`Doggies String :: *`

### 4. 
What is the type of `Husky 10`?

---

`Num a => Doggies a`

### 5. 
What is the type of `Husky (10 :: Integer)`? 

---

`Doggies Integer`

### 6. 
What is the type of `Mastiff "Scooby Doo"`?

---

`Doggies String`

### 7. 
Is `DogueDeBordeaux` a type constructor or a data constructor? 

---

If it's in a type definition it'll be a type constructor, in any other situation it'll be a data constructor.

### 8. 
What is the type of `DogueDeBordeaux`?

---

`DogueDeBordeaux :: a -> DogueDeBordeaux a`


### 9. 

What is the type of DogueDeBordeaux "doggie!"

`DogueDeBordeaux String`




## Exercises: Vehicles (pg.397)

> Given what's in `vehicles.hs`, answer the following:

### 1.
What is the type of `myCar`?

---

`Vehicle`

### 2.
Given the following, define the functions:

```hs
isCar :: Vehicle -> Bool 
isCar = undefined

isPlane :: Vehicle -> Bool 
isPlane = undefined

areCars :: [Vehicle] -> [Bool] 
areCars = undefined
```

---

```hs
isCar :: Vehicle -> Bool 
isCar v = case v of
  Car _ _ -> True
  _ -> False

isPlane :: Vehicle -> Bool 
isPlane p = case p of
  Plane _ -> True
  _ -> False

areCars :: [Vehicle] -> [Bool] 
areCars = map isCar
```

### 3. 
Now, we’re going to write a function to tell us the manufacturer of a piece of data:

```hs
getManu :: Vehicle -> Manufacturer 
getManu = undefined
```
---

```hs
getManu :: Vehicle -> Manufacturer 
getManu v = case v of
  Car m _ -> m
  _ -> error "only Cars have manufacturers"
```

### 4.
Given that we’re returning the `Manufacturer`, what will happen if you use this on `Plane` data?

---

The function would throw an error `"only Cars have manufacturers"`

### 5.
All right. Let’s say you decide to add the size of the plane as an argument to the `Plane` constructor. Add that to your datatypes in the appropriate places, and change your data and functions appropriately.

---

> See `vehicles2.hs`.

```hs
-- relevant changes
data PlaneSize
  = GroupI
  | GroupII
  | GroupIII
  | GroupIV
  | GroupV
  | GroupVI
  deriving (Eq, Show)

data Vehicle 
  = Car Manufacturer Price
  | Plane Airline PlaneSize
  deriving (Eq, Show)

doge = Plane PapuAir GroupV

isPlane :: Vehicle -> Bool 
isPlane p = case p of
  Plane _ _ -> True
  _ -> False
```



## Exercises: Cardinality (pg.402)

### 1. 
```hs
data PugType = PugData
```

---

Cardinality: 1

### 2. 
```hs
     data Airline =
            PapuAir
          | CatapultsR'Us
          | TakeYourChancesUnited
```

---

Cardinality: 3

### 3. 
Given what we know about `Int8`, what’s the cardinality of `Int16`?

---

```
λ> maxBound::Int16
32767
λ> minBound::Int16
-32768
```

Cardinality: 32767 + 32768 + 1 = 65536

### 4. 
Use the REPL and maxBound and minBound to examine Int and Integer. What can you say about the cardinality of those types?

---

- `Int`'s arity is very large: 
9223372036854775808 + 9223372036854775807 + 1 = 18,446,744,073,709,551,616
- `Integer`'s doesn't have maxBound and minBound, so I guess arity is infinite.

### 5. 
Extra credit (impress your friends!): what’s the connection between the 8 in Int8 and that type’s cardinality of 256?

---

It's the power of 2: `2^8 = 256`. The memory size an instance of this type occupies, I guess.




## Exercises: For example (pg.403)

Given

```hs
data Example = MakeExample deriving Show
```

### 1. 

What is the type of the data constructor `MakeExample`? What happens when you request the type of `Example`?

---

- The type of the data constructor `MakeExample` is `Example`.
- The type of `Example` can't be requested because it is a type already. The command crashes because it tries to find a data constructor in scope named `Example`.

### 2. 
What if you try `:info` on `Example` in GHCi? Can you determine what type class instances are defined for the `Example` type using `:info` in GHCi?

---

- `:info Example` tells its members, and also which type classes `Example` has: `Show` in this case.

### 3. 
Try making a new datatype like Example but with a single type argument added to `MakeExample`, such as `Int`. What has changed when you query `MakeExample` with `:type` in GHCi?

---

```hs
data Example2 = MakeExample2 Int deriving Show
```

Querying `MakeExample2` returns `MakeExample2 :: Int -> Example2` because it expects an argument. It's a partially applied unary data constructor.



## Exercises: Logic goats (408)

### 1. 
Reusing the `TooMany` type class, write an instance of the type class for the type `(Int, String)`. This will require adding a language pragma named `FlexibleInstances` if you do not use a newtype GHC will tell you what to do.

---

```hs
newtype I2S = I2S (Int, String)

instance TooMany I2S where
  tooMany (I2S (n, _)) = n > 42

-- Examples:

tooMany $ I2S (42, "") -- False
tooMany $ I2S (43, "") -- True
```

### 2. 
Make another `TooMany` instance for `(Int, Int)`. Sum the values together under the assumption that this is a count of goats from two fields.

---

```hs
newtype I2I = I2I (Int, Int)

instance TooMany I2I where
  tooMany (I2I (n1, n2)) = n1 + n2 > 42

-- Examples:

tooMany $ I2I (40, 2) -- False
tooMany $ I2I (40, 3) -- True
```

### 3. 
Make another `TooMany` instance, this time for `(Num a, TooMany a) => (a, a)`. This can mean whatever you want, such as summing the two numbers together.

---

```hs
newtype N2T a = N2T (a, a)

instance (Num a, TooMany a) => TooMany (N2T a) where
  tooMany (N2T (x, y)) = tooMany x && tooMany y

```



## Exercises: Pity the Bool (pg.410)

### 1. 
Given a datatype:
```hs
data BigSmall 
  = Big Bool
  | Small Bool 
  deriving (Eq, Show)
```

What is its cardinality?

---

Cardinality: `4`

### 2. 
Given a datatype:
```hs
-- bring Int8 in scope
import Data.Int
data NumberOrBool 
  = Numba Int8
  | BoolyBool Bool 
  deriving (Eq, Show)
myNumba = Numba (-128)
```

What is the cardinality of `NumberOrBool`? What happens if you try to create a `Numba` with a numeric literal larger than `127`? And with a numeric literal smaller than `-128`?

---

- Cardinality: 128 + 127 + 1 + 2 = `258`
- Larger than 127 or lower than -128 will throw an _out of bounds_ error.



## Exercises: How does your garden grow? (pg.417)

### 1. 
Given the type:
```hs
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show
type Gardener = String
data Garden =
  Garden Gardener FlowerType
  deriving Show
```

What is the sum of products normal form of Garden?

---

```hs
data Garden 
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show
```



## Exercise: Programmers (pg.426)
Write a function that generates all possible values of `Programmer`. Use the provided lists of inhabitants of `OperatingSystem` and `ProgLang`:

```hs
allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill , Mac
  , Windows
  ]
allLanguages :: [ProgLang] 
allLanguages =
  [Haskell, Agda, Idris, PureScript]
```

---

```hs
-- programmers.hs

allProgrammers :: [Programmer]
allProgrammers 
  = [ Programmer os lang
  | os <- allOperatingSystems
  , lang <- allLanguages
  ]
```



## Exercises: The Quad (pg.434)
Determine how many unique inhabitants each type has. Suggestion: do the arithmetic, unless you want to verify. Writing them out gets tedious quickly:

---

### 1. 
```hs
data Quad 
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)
-- How many different forms can this take?
eQuad :: Either Quad Quad
eQuad = ???
```

--- 

```hs
2 * 4 = 8
-- Right One|Two|Three|Four
-- Left One|Two|Three|Four
```

### 2. 
```prodQuad :: (Quad, Quad)```

---

```hs
4 * 4 = 16
(One|Two|Three|Four, One|Two|Three|Four)
```

### 3. 
```funcQuad :: Quad -> Quad```

---

`4 ^ 4 = 256`

### 4. 
```prodTBool :: (Bool, Bool, Bool)```

---

`2 * 2 * 2 = 8`

### 5. 
```gTwo :: Bool -> Bool -> Bool```

---

`(2 ^ 2) ^ 2 = 16`

### 6. 
```
Hint: five digit number
     fTwo :: Bool -> Quad -> Quad
```

---

`(4 ^ 4) ^ 2 = 65536`



## Write map for BinaryTree (pg.442)

```hs
mapTree :: (a -> b)
  -> BinaryTree a
  -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left x right) 
  = Node (mapTree f left) (f x) (mapTree f right)
```



## Convert binary trees to lists (pg.444)

```hs
preorder :: BinaryTree a -> [a] 
preorder Leaf = []
preorder (Node left x right) 
  = x 
  : (preorder left) 
  ++ (preorder right)


inorder :: BinaryTree a -> [a] 
inorder Leaf = []
inorder (Node left x right) 
  = (inorder left) 
  ++ [x] 
  ++ (inorder right)

postorder :: BinaryTree a -> [a] 
postorder Leaf = []
postorder (Node left x right) 
  = (postorder left) 
  ++ (postorder right) 
  ++ [x]
```



## Write foldr for BinaryTree (pg.445)

```hs
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = 
  foldTree f (foldTree f (f a b) left) right
```



## Chapter exercises (pg.446)

### Multiple choice

### 1. 
Given the following datatype:

```hs
data Weekday 
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
```

Which of the following is true?

- a) Weekday is a type with five data constructors. 
- b) Weekday is a tree with five branches.
- c) Weekday is a product type.
- d) Weekday takes five arguments.

---

- only **a)** is true.

### 2.
With the same datatype definition in mind, what is the type of the following function, `f`?

```hs
f Friday = "Miller Time"
```

- a) `f :: [Char]`
- b) `f :: String -> String`
- c) `f :: Weekday -> String `
- d) `f :: Day -> Beer`

---

- It's **c)**.

### 3.
Types defined with the data keyword:
- a) Must have at least one argument. 
- b) Must begin with a capital letter.
- c) Must be polymorphic.
- d) Cannot be imported from modules.

---

- **b)** is the only one that is true.

### 4.
The function :

```hs
g xs = xs !! (length xs - 1)
```

- a) Is recursive and may not terminate. 
- b) Returns the head of `xs`.
- c) Returns the final element of `xs`. 
- d) Has the same type as `xs`.

---

- **c)** is true.



## Ciphers (pg.447)

> See `ciphers.hs`



## As-patterns (pg.448)

Use as-patterns to implement the following functions:

### 1. 
This should return `True` if (and only if) all the values in the first list appear in the second list, though they need not be contiguous:

```hs
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
```

---

```hs
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf (x:xs) l@(y:ys) = isSubseqOf xs l && elem x l
```

### 2.
Split a sentence into words, then tuple each one with its capitalized form:

```hs
capitalizeWords :: String -> [(String, String)]
```

```
Prelude> capitalizeWords "hello world"
[("hello", "Hello"), ("world", "World")]
```

---

```hs
import Data.Char as Char

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords s 
  = map 
  (\w@(x:xs) -> (w, (Char.toUpper x) : xs)) 
  $ words s 
```



## Language exercises (pg.449)

### 1. 
Write a function that capitalizes a word:

```hs
capitalizeWord :: String -> String 
capitalizeWord = undefined
```

Example output:
```
Prelude> capitalizeWord "Chortle"
"Chortle"
Prelude> capitalizeWord "chortle"
"Chortle"
```

---

```hs
capitalizeWord :: String -> String 
capitalizeWord "" = ""
capitalizeWord (x:xs) = (Char.toUpper x) : xs
```

### 2. 
Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods. Reuse the `capitalizeWord` function:

```hs
capitalizeParagraph :: String -> String 
capitalizeParagraph = undefined
```

```
 Prelude> s = "blah. woot ha."
Prelude> capitalizeParagraph s
"Blah. Woot ha."
```

---

```hs
capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (x:xs) = flow (Char.toUpper x : xs) False ""

flow :: String -> Bool -> String -> String
flow []       _     acc = acc
flow ('.':xs) _     acc = flow xs True (acc ++ ".")
flow (' ':xs) True  acc = flow xs True (acc ++ " ")
flow (x:xs)   True  acc = flow xs False (acc ++ [Char.toUpper x])
flow (x:xs)   False acc = flow xs False (acc ++ [x])
```


## Phone exercise (pg.450)

> See `phone.hs`



## Hutton’s Razor (pg.450)

### 1. 
Your first task is to write the “eval” function that reduces an expression to a final sum:

```hs
data Expr
       = Lit Integer
       | Add Expr Expr

eval :: Expr -> Integer 
eval = error "do it to it"
```

Example of expected output:

```
Prelude> eval (Add (Lit 1) (Lit 9001))
9002
```

---

```hs
eval :: Expr -> Integer 
eval (Lit n) = n
eval (Add ex ex') = eval ex + eval ex'
```

### 2.
Write a printer for the expressions:

```hs
printExpr :: Expr -> String 
printExpr = undefined
```

Expected output:
```
Prelude> printExpr (Add (Lit 1) (Lit 9001))
"1 + 9001"
Prelude> a1 = Add (Lit 9001) (Lit 1)
Prelude> a2 = Add a1 (Lit 20001)
Prelude> a3 = Add (Lit 1) a2
Prelude> printExpr a3
"1 + 9001 + 1 + 20001"
```

---

```hs
printExpr :: Expr -> String 
printExpr (Lit n) = show n
printExpr (Add ex ex') 
  = List.intercalate 
  " + " 
  [printExpr ex, printExpr ex']
```
