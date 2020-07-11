module Cipher (basicCaesar, basicUncaesar) where

import Data.Char

-- Generic encoder/decoder

cipher :: (Char -> Char) -> String -> String
cipher fn s = map fn s

-- Basic Caesar cipher algorithm, shifted 3 chars

rShift = 1
ordA = 97
azIndex ch =  ord ch - ordA
ch_a = 0
ch_z = 25
az = ['a'..'z']

basicEncoder :: Char -> Char
basicEncoder ' ' = ' '
basicEncoder ch = 
  let
    nextChar = azIndex ch + rShift 
    wrapped = nextChar `mod` (ch_z+1)
  in
    az !! wrapped

basicDecoder :: Char -> Char
basicDecoder  ' ' = ' '
basicDecoder ch = 
  let
    prevChar = azIndex ch - rShift 
    wrapped = 
      if prevChar < ch_a
      then (ch_z+1) + prevChar
      else prevChar
  in
    az !! wrapped

-- Public 

basicCaesar = cipher basicEncoder
basicUncaesar = cipher basicDecoder