module Exercises where
import Data.Char as Char
import Data.List as List

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf (x:xs) l@(y:ys) = isSubseqOf xs l && elem x l

resultOk
  =  isSubseqOf "blah" "blahwoot" == True
  && isSubseqOf "blah" "wootblah" == True
  && isSubseqOf "blah" "wboloath" == True
  && isSubseqOf "blah" "wootbla" == False
  && isSubseqOf "blah" "halbwoot" == True
  && isSubseqOf "blah" "blawhoot" == True
  && isSubseqOf "blah" "blawoot" == False

---

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords s 
  = map 
  (\w@(x:xs) -> (w, (Char.toUpper x) : xs)) 
  $ words s 

---

capitalizeWord :: String -> String 
capitalizeWord "" = ""
capitalizeWord (x:xs) = (Char.toUpper x) : xs

---

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (x:xs) = flow (Char.toUpper x : xs) False ""

flow :: String -> Bool -> String -> String
flow []       _     acc = acc
flow ('.':xs) _     acc = flow xs True (acc ++ ".")
flow (' ':xs) True  acc = flow xs True (acc ++ " ")
flow (x:xs)   True  acc = flow xs False (acc ++ [Char.toUpper x])
flow (x:xs)   False acc = flow xs False (acc ++ [x])

---

data Expr
       = Lit Integer
       | Add Expr Expr

eval :: Expr -> Integer 
eval (Lit n) = n
eval (Add ex ex') = eval ex + eval ex'

printExpr :: Expr -> String 
printExpr (Lit n) = show n
printExpr (Add ex ex') 
  = List.intercalate 
  " + " 
  [printExpr ex, printExpr ex']
