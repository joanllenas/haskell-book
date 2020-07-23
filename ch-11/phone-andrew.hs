module DaPhoneAndrew where

import           Data.Char
import           Data.Int
import           Data.List (group, intercalate, maximumBy, sort)
import           Data.Function

-- Phone exercise

-- 1. Create a data structure that captures the phone layout above

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone (Char -> [(Digit, Presses)])

mkDaPhone = DaPhone f
  where f 'a' = [('2', 1)]
        f 'b' = [('2', 2)]
        f 'c' = [('2', 3)]
        f 'd' = [('3', 1)]
        f 'e' = [('3', 2)]
        f 'f' = [('3', 3)]
        f 'g' = [('4', 1)]
        f 'h' = [('4', 2)]
        f 'i' = [('4', 3)]
        f 'j' = [('5', 1)]
        f 'k' = [('5', 2)]
        f 'l' = [('5', 3)]
        f 'm' = [('6', 1)]
        f 'n' = [('6', 2)]
        f 'o' = [('6', 3)]
        f 'p' = [('7', 1)]
        f 'q' = [('7', 2)]
        f 'r' = [('7', 3)]
        f 's' = [('7', 4)]
        f 't' = [('8', 1)]
        f 'u' = [('8', 2)]
        f 'v' = [('8', 3)]
        f 'w' = [('9', 1)]
        f 'x' = [('9', 2)]
        f 'y' = [('9', 3)]
        f 'z' = [('9', 4)]
        f ' ' = [('0', 2)]
        f '.' = [('#', 1)]
        f ',' = [('#', 2)]
        f '1' = [('1', 1)]
        f '2' = [('2', 4)]
        f '3' = [('3', 4)]
        f '4' = [('4', 4)]
        f '5' = [('5', 4)]
        f '6' = [('6', 4)]
        f '7' = [('7', 5)]
        f '8' = [('8', 4)]
        f '9' = [('9', 5)]
        f x = if isUpper x then ('*', 1) : f (toLower x) else []

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone f) c = f c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone s = s >>= reverseTaps phone

-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map (snd)

-- 4. What was the most popular letter for each message? What was its cost?
mostPopularLetter :: String -> Char
mostPopularLetter = mostCommon

mostPopularLetterCost :: DaPhone -> String -> Presses
mostPopularLetterCost (DaPhone f) = fingerTaps . f . mostCommon

-- 5. What was the most popular letter overall? What was the most popular word?
coolestLtr :: [String] -> Char
coolestLtr = mostCommon . concat

coolestWord :: [String] -> String
coolestWord = mostCommon

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

