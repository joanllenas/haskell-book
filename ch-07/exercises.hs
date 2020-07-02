module Exercises where

functionC x y = if (x > y) then x else y
functionC2 x y = case x > y of
  True -> x
  False -> y
testFunctionC x y = (functionC x y) == (functionC2 x y)

ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2_2 n = case even n of
  True -> (n+2)
  False -> n
testIfEvenAdd2 n = (ifEvenAdd2 n) == (ifEvenAdd2_2 n)

nums x =
  case compare x 0 of
    LT -> -1 
    GT -> 1
    EQ -> 0

data Employee 
  = Coder
  | Manager
  | Veep
  | CEO 
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' 
  = putStrLn 
  $ show e
  ++ " is the boss of " 
  ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e'= 
  case compare e e' of
    GT -> reportBoss e e'
    LT -> (flip reportBoss) e e'
    EQ -> putStrLn "Neither Employee is the boss"


(>>>) = flip (.)
res 
  = enumFrom 
  >>> filter odd 
  >>> map (+1)
  >>> take 5 
  $ 3


tensDigit :: Integral a => a -> a 
tensDigit x = d
  where 
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' 
  = (flip mod $ 10) 
  . fst 
  . (flip divMod $ 10)

hunsD :: Integral a => a -> a
hunsD   
  = (flip mod $ 10) 
  . (flip div $ 100)


foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y bool = 
  case bool of
    True -> y
    False -> x

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y bool
  | bool == True = y
  | bool == False = x


g :: (a -> b) -> (a, c) -> (b, c) 
g a2b (a, c) = (a2b a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show
