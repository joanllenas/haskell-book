module DaPhone where

import qualified Data.Char as C
import qualified Data.Map.Strict as M
import Data.Maybe

data Button
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Zero
  | Star
  | Hash
  deriving (Eq, Show, Ord, Enum, Bounded)

type Presses = Int

type CharCode = (Button, Presses)

data TapCode
  = LowerCase CharCode
  | UpperCase CharCode
  deriving (Show, Eq)

instance Ord TapCode where
  compare (UpperCase _) (LowerCase _) = LT
  compare (LowerCase _) (UpperCase _) = GT
  compare (LowerCase x) (LowerCase y) = compare x y
  compare (UpperCase x) (UpperCase y) = compare x y

type TapMap = M.Map Char CharCode

tapMap :: TapMap
tapMap =
  M.fromList
    [ ('1', (One, 1)),
      ('a', (Two, 1)),
      ('b', (Two, 2)),
      ('c', (Two, 3)),
      ('2', (Two, 4)),
      ('d', (Three, 1)),
      ('e', (Three, 2)),
      ('f', (Three, 3)),
      ('3', (Three, 4)),
      ('g', (Four, 1)),
      ('h', (Four, 2)),
      ('i', (Four, 3)),
      ('4', (Four, 4)),
      ('j', (Five, 1)),
      ('k', (Five, 2)),
      ('l', (Five, 3)),
      ('5', (Five, 4)),
      ('m', (Six, 1)),
      ('n', (Six, 2)),
      ('o', (Six, 3)),
      ('6', (Six, 4)),
      ('p', (Seven, 1)),
      ('q', (Seven, 2)),
      ('r', (Seven, 3)),
      ('s', (Seven, 4)),
      ('7', (Seven, 5)),
      ('t', (Eight, 1)),
      ('u', (Eight, 2)),
      ('v', (Eight, 3)),
      ('8', (Eight, 4)),
      ('w', (Nine, 1)),
      ('x', (Nine, 2)),
      ('y', (Nine, 3)),
      ('z', (Nine, 4)),
      ('9', (Nine, 5)),
      ('^', (Star, 2)),
      ('*', (Star, 3)),
      ('+', (Zero, 1)),
      (' ', (Zero, 2)),
      ('0', (Zero, 3)),
      ('.', (Hash, 1)),
      (',', (Hash, 2)),
      ('#', (Hash, 3))
    ]

encodeCharacter :: TapMap -> Char -> Maybe TapCode
encodeCharacter tm c =
  if C.isUpper c
    then UpperCase <$> M.lookup (C.toLower c) tm
    else LowerCase <$> M.lookup c tm

encodeString :: TapMap -> String -> [Maybe TapCode]
encodeString tm s = encodeCharacter tm <$> s

tapCode2CharCode :: TapCode -> [CharCode]
tapCode2CharCode (UpperCase cc) = [(Star, 1), cc]
tapCode2CharCode (LowerCase cc) = [cc]

typeText :: TapMap -> String -> [CharCode]
typeText tm t = concat $ mapMaybe (fmap tapCode2CharCode) $ encodeString tm t

pressCount :: [CharCode] -> Presses
pressCount = foldr (\(b, p) a -> p + a) 0

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

encodeConvo :: [TapCode]
encodeConvo = catMaybes $ concatMap (encodeString tapMap) convo

typeConvo :: [[CharCode]]
typeConvo = fmap (typeText tapMap) convo

-- Returns a map with the distinct elements of the given list as keys and their
-- occurence count as values.
countElements :: Ord a => [a] -> M.Map a Int
countElements xs = foldr counter M.empty xs
  where
    counter x m = M.insertWith (+) x 1 m

-- Retrieve the element with the highest count.
maxCount :: Ord a => M.Map a Int -> (a, Int)
maxCount m = M.foldrWithKey maxer (head $ M.toList m) m
  where
    maxer :: Ord a => a -> Int -> (a, Int) -> (a, Int)
    maxer k c (k', currentMax) =
      if c > currentMax
        then (k, c)
        else (k', currentMax)

mostPopularLetters :: [(Char, Int)]
mostPopularLetters = maxCount . countElements <$> convo

mostPopularLetterOverall :: (Char, Int)
mostPopularLetterOverall = maxCount . countElements $ concat convo

mostPopularWord :: (String, Int)
mostPopularWord = maxCount . countElements $ concat $ words <$> convo
