module StringProcessing where

import Data.List as List
import Data.Bool

notThe :: String -> Maybe String 
notThe "the" = Nothing
notThe s = Just s

the2a :: Maybe String -> String
the2a Nothing = "a"
the2a (Just s) = s

replaceThe :: String -> String 
replaceThe s 
  = List.intercalate " " 
  . map the2a
  . map notThe
  $ words s


---

isVowel :: Char -> Bool
isVowel ch = elem ch ['a','e','i','o','u']

countTheBeforeVowel :: String -> Integer 
countTheBeforeVowel s 
  = snd
  . foldr
    (\w (x,c) -> (w, bool c (c+1) w == "the" && isVowel(x!!0) )) 
    ("", 0)
    (words s)