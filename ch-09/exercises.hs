import Data.Char
import Data.List

-- Exercise: EnumFromTo (pg306)
eftBool :: Bool -> Bool -> [Bool] 
eftBool False False = [False]
eftBool True True = [True]
eftBool True False = [True, False]
eftBool False True = []


myFromTo :: (Eq a, Ord a, Enum a) => a -> a -> [a] -> [a]
myFromTo from to xs 
  | from > to = []
  | from == to = from : xs
  | otherwise = myFromTo (succ from) to (from : xs)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to = reverse $ myFromTo from to []


eftInt :: Int -> Int -> [Int] 
eftInt from to = reverse $ myFromTo from to []

eftChar :: Char -> Char -> [Char] 
eftChar from to = reverse $ myFromTo from to []


-- Exercises: Thy fearful symmetry (pg.310)

-- 1.
lTrim :: String -> String
lTrim [] = ""
lTrim (' ':xs) = lTrim xs
lTrim (x:xs) = x : xs

myWords :: String -> [String]
myWords "" = []
myWords s = 
  let
    str = takeWhile (\ch -> ch /= ' ') s
    rest = dropWhile (\ch -> ch /= ' ') s
  in
    [str] ++ myWords (lTrim rest)

-- 3.

ltrimChar :: Char -> String -> String
ltrimChar _ [] = ""
ltrimChar ch (x:xs) = 
  if ch == x 
  then ltrimChar ch xs 
  else x : xs

splitWords :: Char -> String -> [String]
splitWords _ "" = []
splitWords char s = 
  let
    str = takeWhile (\ch -> ch /= char) s
    rest = dropWhile (\ch -> ch /= char) s
  in
    [str] ++ splitWords char (ltrimChar char rest)


-- 

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) 
  | e == x = True 
  | otherwise = myElem e xs

--

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "you must provide at least one element"
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = 
  let
    gt  = filter (\e -> f e x == GT) xs
  in
    if length gt == 0 -- there were only `x` duplicates, so the filter operation removed everyhitng from xs
    then x
    else myMaximumBy f gt


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "you must provide at least one element"
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = 
  let
    lt  = filter (\e -> f e x == LT) xs
  in
    if length lt == 0 -- there were only `x` duplicates, so the filter operation removed everyhitng from xs
    then x
    else myMinimumBy f lt

    
myMaximum :: (Ord a) => [a] -> a 
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a 
myMinimum xs = myMinimumBy compare xs