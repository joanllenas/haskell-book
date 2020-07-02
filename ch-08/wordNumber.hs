module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n >= 0 && n < 10 = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! n
  | otherwise        = error "n must be between 0 and 9."

digits :: Int -> [Int]
digits num = 
  numAcc []
  where
    numAcc xs = 
      let
        divisor = 10 ^ (length xs)
        digit = num `div` divisor `mod` 10
      in
        if divisor <= num
        then numAcc $ digit : xs
        else xs

(|>) = flip ($)

wordNumber :: Int -> String
wordNumber n 
  = digits n
  |> map digitToWord
  |> intersperse "-"
  |> concat
  

{-
digits 123

  numAcc []
    let
      divisor = 10^0 = 1
      digit   = 123 `div` 1 `mod` 10 => 3
    in
      10 < 123 => numAcc [2]
  
  numAcc [2]
    let
      divisor = 10^1 = 10
      digit   = 123 `div` 10 `mod` 10 => 2
    in
      100 < 123 => numAcc [1,2]

  numAcc [1,2]
    let
      divisor = 10^2 = 100
      digit   = 123 `div` 100 `mod` 10 => 1
    in
      100 < 123 => numAcc [1,2,3]

  numAcc [1,2,3]
    let
      divisor = 10^3 = 1000
      digit   = 123 `div` 1000 `mod` 10 => 0
    in
      1000 > 123 => [1,2,3]
-}
