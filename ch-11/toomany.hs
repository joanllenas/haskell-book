module TooMany where

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

class TooMany a where 
  tooMany :: a -> Bool

instance TooMany Int where 
  tooMany n = n > 42

instance TooMany Goats where 
  tooMany (Goats n) = n > 42

-- 1.

newtype I2S = I2S (Int, String)

instance TooMany I2S where
  tooMany (I2S (n, _)) = n > 42

ex1_False = tooMany $ I2S (42, "")
ex1_True = tooMany $ I2S (43, "")

-- 2. 

newtype I2I = I2I (Int, Int)

instance TooMany I2I where
  tooMany (I2I (n1, n2)) = n1 + n2 > 42

ex2_False = tooMany $ I2I (40, 2)
ex2_True = tooMany $ I2I (40, 3)

-- 3.

newtype N2T a = N2T (a, a)

instance (Num a, TooMany a) => TooMany (N2T a) where
  tooMany (N2T (x, y)) = tooMany x && tooMany y
