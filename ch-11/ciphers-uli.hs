module Vigenere where

import qualified Data.Char as C
import qualified Data.List as L

base :: Int
base = C.ord 'A'

c :: Int
c = length ['A' .. 'Z']

-- Normalize the input
normalizeInput :: String -> String
normalizeInput s = C.toUpper <$> (unwords . words $ s)

prepareKey :: String -> String
prepareKey k = L.cycle preKey
  where
    preKey = normalizeInput k

vigEnc :: String -> String -> String
vigEnc plaintext key = encode p k
  where
    p = normalizeInput plaintext
    k = prepareKey key

encode :: String -> String -> String
encode [] _ = []
encode (p : ps) kk@(k : ks)
  | p == ' ' = ' ' : encode ps kk
  | otherwise = encodeChar p k : encode ps ks

encodeChar :: Char -> Char -> Char
encodeChar p k = C.chr wrapped
  where
    pBase = C.ord p - base
    kBase = C.ord k - base
    shifted = pBase + kBase
    wrapped = (shifted `mod` c) + base
