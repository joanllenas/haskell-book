module OnlyNatural where

data Nat 
  = Zero
  | Succ Nat
  deriving (Eq, Show)

--

natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + (natToInteger nat)

--

getNat :: Maybe Nat -> Nat
getNat Nothing = Zero
getNat (Just nat) = Succ nat

integerToNat :: Integer -> Maybe Nat 
integerToNat n
  | n == 0 = Just Zero
  | n < 0 = Nothing
  | otherwise = Just $ getNat $ integerToNat (n-1)
