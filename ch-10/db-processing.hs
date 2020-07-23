module DbProcessing where

import Data.Time
import Data.List

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime 
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem] 
theDatabase 
  = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)) 
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

---

utcTime :: DatabaseItem -> [UTCTime]
utcTime item 
  = case item of
    DbString _ -> []
    DbNumber _ -> []
    DbDate t -> [t]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = foldr (\a b -> (utcTime a) ++ b) [] db

---

integer :: DatabaseItem -> [Integer]
integer item 
  = case item of
    DbString _ -> []
    DbNumber n -> [n]
    DbDate _ -> []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr (\a b -> (integer a) ++ b) [] db

---

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = head . reverse . sort . filterDbDate $ db

---

sumDb :: [DatabaseItem] -> Integer
sumDb db = sum . filterDbNumber $ db

---

avg :: [Integer] -> Double
avg xs = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)

avgDb :: [DatabaseItem] -> Double
avgDb db = avg . filterDbNumber $ db
