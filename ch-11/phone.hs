module Phone where

import Data.Char as Char
import Data.List as List
import Data.Function -- needed because of `on`

type Digit = Char
type Presses = Int
type Tap = (Digit, Presses)
type TapAcc = (Digit, Int) -- Tap accumulator
type Key = (Char, [Char])
uc :: Char
uc = chr 0 -- Uppercase char


-- -- --
-- 1. --
-- -- --


keypad :: [Key] -- 1.
keypad = 
  [ ('0', [' ', '0'])
  , ('1', ['1'])
  , ('2', ['a', 'b', 'c', '2'])
  , ('3', ['d', 'e', 'f', '3'])
  , ('4', ['g', 'h', 'i', '4'])
  , ('5', ['j', 'k', 'l', '5'])
  , ('6', ['m', 'n', 'o', '6'])
  , ('7', ['p', 'q', 'r', 's', '7'])
  , ('8', ['t', 'u', 'v', '8'])
  , ('9', ['w', 'x', 'y', 'z', '9'])
  , ('*', [uc, '*'])
  , ('#', ['.', ',', '#'])
  ]

-- utils

uppercaseTap :: Tap
uppercaseTap = ('*', 1)

charsFromKey :: Key -> [Char]
charsFromKey (_, chs) = chs

keyCharFromKey :: Key -> Char
keyCharFromKey (ch, _) = ch

keyHasChar :: Key -> Char -> Bool
keyHasChar (_, chs) ch 
  = elem (toLower ch) chs

incrPresses :: Tap -> Tap
incrPresses (d, p) = (d, p+1)

findKeyWithChar :: [Key] -> Char -> Key
findKeyWithChar kpad ch
  = fst -- Key
  . head -- (Key, True)
  . filter (\(_, hasChar) -> hasChar) -- [(Key, True)]
  . map (\k -> (k, keyHasChar k ch)) -- [(Key, Bool)]
  $ kpad


-- -- --
-- 2. --
-- -- --


convo :: [String] 
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
tapsForChar :: [Key] -> Char -> [Tap]
tapsForChar kpad ch =
  let
    key :: Key
    key = findKeyWithChar kpad ch
    keyChar :: Char
    keyChar = keyCharFromKey key
    tap :: Tap
    tap 
      = snd -- extract the Tap from (Bool, Tap)
      . foldr
        (\ch' acc@(found, t) 
          -> if found 
            then acc
            else (ch' == toLower ch , incrPresses t)
        ) -- Accumulate presses for the given keyChar while char is not reached
        (False, (keyChar, 0)) -- Initial value: (Bool<Char Was found>, Tap<Accumulated Taps>)
      $ reverse -- it's foldr, and we want to go from left to right
      $ charsFromKey key -- [Char]
  in
    if Char.isUpper ch 
    then uppercaseTap : [tap] -- Add an uppercase tap at the beginning if it's uppercase
    else [tap]

tapsForString :: [Key] -> String -> [Tap]
tapsForString kpad s 
  = foldr 
  (\ch taps -> taps ++ (tapsForChar kpad ch)) 
  [] 
  $ reverse s

convoToTaps :: [[Tap]] -- 2.
convoToTaps 
  = foldr 
  (\s acc -> (tapsForString keypad s) : acc) 
  [] 
  convo

-- λ> convoToTaps
-- [[('*',1),('9',1),('2',1),('6',2),('6',2),('2',1),('0',1),('7',1),('5',3),...,('8',2),('7',3),('0',1),('8',1),('8',2),('7',3),('6',2)]]


-- -- --
-- 3. --
-- -- --


fingerTaps :: [Tap] -> Presses 
fingerTaps taps 
  = foldr 
  (\(_, p) acc -> acc + p) 
  0 
  taps

convoToPresses :: [Presses] -- 3.
convoToPresses
  = foldr
  (\taps ps -> (fingerTaps taps) : ps)
  []
  convoToTaps

-- λ> convoToPresses
-- [48,5,17,73,15,49,59,15,60]

-- -- --
-- 4. --
-- -- --

mostPopularLetterWithCost :: String -> TapAcc
mostPopularLetterWithCost s 
  = (\(ch, n) -> (ch, n * (length $ tapsForChar keypad ch))) -- (char, num taps for all occurences)
  . (\l@(x:_) -> (x, length l)) -- (char, num char occurences)
  . maximumBy (\a b -> compare (length a) (length b)) -- pick longest list
  . List.groupBy (==) -- ["aabcc"] -> [['a','a'],['b'],['c','c']]
  . sort -- needed by groupBy to work
  $ s

convoToPopularLetterWithCost :: [String] -> [TapAcc] -- 4
convoToPopularLetterWithCost
  = foldr 
  (\s xs -> (mostPopularLetterWithCost s) : xs) 
  []

-- λ> convoToPopularLetterWithCost convo
-- [('n',3),('a',1),('h',2),(' ',6),('y',1),(' ',5),(' ',7),('y',1),(' ',5)]

-- -- --
-- 5. --
-- -- --


coolestLetter :: [String] -> Char -- 5.1
coolestLetter xs
  = fst
  . maximumBy (compare `on` snd) -- (Char, Int)
  . convoToPopularLetterWithCost -- [(Char, Int)]
  $ xs 

-- λ> coolestLetter convo
-- ' '

coolestWord :: [String] -> String -- 5.2
coolestWord xs = 
  fst
  . maximumBy (compare `on` snd) -- (String, Int)
  . map (\l@(x:_) -> (x, length l)) -- [("aa", 2), ("bb", 1), ("cc", 1)]
  . List.groupBy (==) -- [["aa", "aa"], ["bb"], ["cc"]]
  . sort -- ["aa", "aa", "bb", "cc"]
  . concat -- ["aa", "bb", "cc", "aa"]
  . map words -- [["aa", "bb"], ["cc", "aa"]]
  $ xs -- ["aa bb", "cc aa"]

-- λ> coolestWord convo
-- "Lol"