module Ciphers (caesar, unCaesar, vigenere, unVigenere) where

import           Data.Char

minOrdUpper :: Int
minOrdUpper = ord 'A'

minOrdLower :: Int
minOrdLower = ord 'a'

alphSize :: Int
alphSize = length ['a' .. 'z']

caesar :: Int -> String -> String
caesar n = map $ shift n

unCaesar :: Int -> String -> String
unCaesar n = map $ shift (-n)

shift n c
  | isLower c = shiftMod minOrdLower c
  | isUpper c = shiftMod minOrdUpper c
  | otherwise = c
  where shiftMod m = chr . (+ m) . (`mod` alphSize) . (+ n) . (subtract m) . ord

type Key = String
type Message = String
type EncodedMessage = String

vigenere :: Key -> Message -> EncodedMessage
vigenere = vigenereCodec posShift

unVigenere :: Key -> EncodedMessage -> Message
unVigenere = vigenereCodec negShift

vigenereCodec :: (Char -> Int) -> Key -> String -> String
vigenereCodec f k m  = z
  where x = zipLetters (cycle k) m
        y = map (\ (k, m) -> (f k, m)) x
        z = map (uncurry shift) y

zipLetters :: [Char] -> [Char] -> [(Char, Char)]
zipLetters "" _ = []
zipLetters _ "" = []
zipLetters s1@(h1:t1) s2@(h2:t2)
  | not $ isLetter h1 = (h1, h1) : zipLetters t1 s2
  | not $ isLetter h2 = (h2, h2) : zipLetters s1 t2
  | otherwise = (h1, h2) : zipLetters t1 t2

posShift :: Char -> Int
posShift c
  | isLower c = ord c - minOrdLower
  | isUpper c = ord c - minOrdUpper
  | otherwise = 0

negShift :: Char -> Int
negShift = negate . posShift
