type Digit = Char
type Presses = Int
data DaPhone
  = DaPhone [(Digit, Presses)]
  deriving Show
press :: Char -> DaPhone
press c = case c of
  '1' -> DaPhone [('1', 1)]
  'a' -> DaPhone [('2', 1)]
  'A' -> DaPhone [('*', 1), ('2', 1)]
  'b' -> DaPhone [('2', 2)]
  'B' -> DaPhone [('*', 1), ('2', 2)]
  'c' -> DaPhone [('2', 3)]
  'C' -> DaPhone [('*', 1), ('2', 3)]
  '2' -> DaPhone [('2', 4)]
  'd' -> DaPhone [('3', 1)]
  'D' -> DaPhone [('*', 1), ('3', 1)]
  'e' -> DaPhone [('3', 2)]
  'E' -> DaPhone [('*', 1), ('3', 2)]
  'f' -> DaPhone [('3', 3)]
  'F' -> DaPhone [('*', 1), ('3', 3)]
  '3' -> DaPhone [('3', 4)]
  'g' -> DaPhone [('4', 1)]
  'G' -> DaPhone [('*', 1), ('4', 1)]
  'h' -> DaPhone [('4', 2)]
  'H' -> DaPhone [('*', 1), ('4', 2)]
  'i' -> DaPhone [('4', 3)]
  'I' -> DaPhone [('*', 1), ('4', 3)]
  '4' -> DaPhone [('4', 4)]
  'j' -> DaPhone [('5', 1)]
  'J' -> DaPhone [('*', 1), ('5', 1)]
  'k' -> DaPhone [('5', 2)]
  'K' -> DaPhone [('*', 1), ('5', 2)]
  'l' -> DaPhone [('5', 3)]
  'L' -> DaPhone [('*', 1), ('5', 3)]
  '5' -> DaPhone [('5', 4)]
  'm' -> DaPhone [('6', 1)]
  'M' -> DaPhone [('*', 1), ('6', 1)]
  'n' -> DaPhone [('6', 2)]
  'N' -> DaPhone [('*', 1), ('6', 2)]
  'o' -> DaPhone [('6', 3)]
  'O' -> DaPhone [('*', 1), ('6', 3)]
  '6' -> DaPhone [('6', 4)]
  'p' -> DaPhone [('7', 1)]
  'P' -> DaPhone [('*', 1), ('7', 1)]
  'q' -> DaPhone [('7', 2)]
  'Q' -> DaPhone [('*', 1), ('7', 2)]
  'r' -> DaPhone [('7', 3)]
  'R' -> DaPhone [('*', 1), ('7', 3)]
  's' -> DaPhone [('7', 4)]
  'S' -> DaPhone [('*', 1), ('7', 4)]
  '7' -> DaPhone [('7', 5)]
  't' -> DaPhone [('8', 1)]
  'T' -> DaPhone [('*', 1), ('8', 1)]
  'u' -> DaPhone [('8', 2)]
  'U' -> DaPhone [('*', 1), ('8', 2)]
  'v' -> DaPhone [('8', 3)]
  'V' -> DaPhone [('*', 1), ('8', 3)]
  '8' -> DaPhone [('8', 4)]
  'w' -> DaPhone [('9', 1)]
  'W' -> DaPhone [('*', 1), ('9', 1)]
  'x' -> DaPhone [('9', 2)]
  'X' -> DaPhone [('*', 1), ('9', 2)]
  'y' -> DaPhone [('9', 3)]
  'Y' -> DaPhone [('*', 1), ('9', 3)]
  'z' -> DaPhone [('9', 4)]
  'Z' -> DaPhone [('*', 1), ('9', 4)]
  '9' -> DaPhone [('9', 5)]
  '+' -> DaPhone [('0', 1)]
  ' ' -> DaPhone [('0', 2)]
  '0' -> DaPhone [('0', 3)]
  '.' -> DaPhone [('#', 1)]
  ',' -> DaPhone [('#', 2)]
  '#' -> DaPhone [('#', 3)]
--press :: Char -> DaPhone
extract :: DaPhone -> [(Digit, Presses)]
extract c = case c of
  DaPhone a -> a
reverseTaps :: Char ->  [(Digit, Presses)]
reverseTaps  = extract . press
cellPhonesDead' :: String -> [(Digit, Presses)]
cellPhonesDead' sentence =
  concat $ map (\x -> reverseTaps x) sentence
cellPhonesDead :: [String] -> [(Digit, Presses)]
cellPhonesDead  dialog =
  concat $ map (\x -> cellPhonesDead' x) dialog
convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]
{-
cellPhonesDead convo
[('*',1),('9',1),('2',1),('6',2),('6',2),('2',1),('0',2),('7',1),('5',3),('2',1),('9',3),('0',2),('2',4),('0',3),('0',2),('7',2),('8',2),('3',2),('7',4),('8',1),('4',3),('6',3),('6',2),('7',4),('*',1),('9',3),('2',1),('*',1),('8',2),('0',2),('1',1),('7',4),('8',1),('0',2),('4',2),('2',1),('4',2),('2',1),('*',1),('5',3),('6',3),('5',3),('0',2),('6',3),('5',2),('#',1),('0',2),('*',1),('4',2),('2',1),('8',3),('3',2),('0',2),('8',2),('0',2),('3',2),('8',3),('3',2),('7',3),('0',2),('8',1),('2',1),('7',4),('8',1),('3',2),('3',1),('0',2),('2',1),('5',3),('2',3),('6',3),('4',2),('6',3),('5',3),('0',2),('5',3),('6',3),('5',3),('*',1),('5',3),('6',3),('5',3),('0',2),('9',3),('2',1),('*',1),('9',1),('6',3),('9',1),('0',2),('8',2),('7',3),('0',2),('2',3),('6',3),('6',3),('5',3),('0',2),('4',2),('2',1),('4',2),('2',1),('#',1),('0',2),('*',1),('8',2),('7',3),('0',2),('8',1),('8',2),('7',3),('6',2),('*',1),('6',3),('5',2),('#',1),('0',2),('*',1),('3',1),('6',3),('0',2),('8',2),('0',2),('8',1),('4',2),('4',3),('6',2),('5',2),('0',2),('*',1),('4',3),('0',2),('2',1),('6',1),('0',2),('7',1),('7',3),('3',2),('8',1),('8',1),('9',3),('0',2),('*',1),('5',3),('6',3),('5',3),('*',1),('5',3),('6',3),('5',3),('0',2),('9',3),('2',1),('*',1),('4',2),('2',1),('4',2),('2',1),('0',2),('8',1),('4',2),('2',1),('6',2),('5',2),('7',4),('0',2),('5',1),('8',2),('7',4),('8',1),('0',2),('6',1),('2',1),('5',2),('4',3),('6',2),('4',1),('0',2),('7',4),('8',2),('7',3),('3',2),('0',2),('7',3),('6',3),('3',3),('5',3),('0',2),('8',2),('7',3),('0',2),('8',1),('8',2),('7',3),('6',2)]
-}