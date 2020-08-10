module Listy where

newtype Listy a 
  = Listy [a] 
  deriving (Eq, Show)

instance Semigroup (Listy a) where 
  (<>) (Listy l) (Listy l') = Listy $ mappend l l'
