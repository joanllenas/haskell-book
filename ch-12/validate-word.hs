module ValidateWord where

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