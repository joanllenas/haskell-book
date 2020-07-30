module Person where

type Name = String
type Age = Integer
data Person 
  = Person Name Age 
  deriving Show
data PersonInvalid 
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String 
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow 
  | otherwise 
      = Left 
      $ PersonInvalidUnknown 
      $ "Name was: " ++ show name 
      ++ " Age was: " ++ show age

---

getResult :: String -> Integer -> String
getResult name age 
  = case mkPerson name age of
    Right p -> "Yay! Successfully got a person: " ++ (show p)
    Left NameEmpty -> "Error: Name can't be an empty string"
    Left AgeTooLow -> "Error: Age must be higher than 0"
    Left (PersonInvalidUnknown s) -> "Unknown Error: " ++ s

gimmePerson :: IO () 
gimmePerson = do
  putStr "What's your name?"
  name <- getLine
  putStr "What's your age?"
  age <- getLine
  let age' = read age :: Integer
  putStrLn $ getResult name age'

