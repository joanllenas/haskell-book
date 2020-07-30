module CipherInput where

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

caesar :: String -> String
caesar = cipher basicEncoder

uncaesar :: String -> String
uncaesar = cipher basicDecoder

-- Input

doEncode :: IO ()
doEncode = do
  putStr "Enter the string you want to encode: "
  s <- getLine
  let s' = caesar s
  putStrLn $ "Your encoded string is: " ++ s'

doDecode :: IO ()
doDecode = do
  putStr "Enter the string you want to decode: "
  s <- getLine
  let s' = uncaesar s
  putStrLn $ "Your decoded string is: " ++ s'

main :: IO ()
main = do
  putStr "Do you want to encode or decode? (e/d)"
  c <- getChar
  putStrLn ""
  case c of
    'e' -> doEncode
    'd' -> doDecode
    _ -> main