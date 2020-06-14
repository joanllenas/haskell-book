module Exercises where

fna :: String -> String
fna x = "Curry is awesome" ++ x

fnb :: Int -> Char
fnb x = "Curry is awesome" !! x

fnc :: Int -> String
fnc x = drop x "Curry is awesome!"
