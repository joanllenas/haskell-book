module UsingQuickCheck where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as List
import qualified Data.Char as Char


-- 1.
{-
-- for a function
half x = x / 2
-- this property should hold
halfIdentity = (*2) . half
-}

half :: Fractional a => a -> a
half x = x / 2

ex1 :: IO ()
ex1 = hspec $ do
  describe "half" $ do
    it "half x * 2 is always x" $ do
      property $ \x ->  x == ((*2) . half $ x::Float)


-- 2.

listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs =
  snd $ foldr go (Nothing, True) xs 
  where go _ status@(_, False) = status 
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

ex2 :: IO ()
ex2 = hspec $ do
  describe "sort" $ do
    it "listOrdered $ sort x should always be true" $ do
      property $ (\x -> (listOrdered $ List.sort (x::[Int])) == True)

-- 3

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

ex3 :: IO ()
ex3 = hspec $ do
  describe "+ associativity" $ do
    it "parenthesization doesn't matter" $ do
      property $ (\x y z -> (plusAssociative (x::Int) y z) == True)
  describe "+ commutative" $ do
    it "order doesn't matter" $ do
      property $ (\x y -> (plusCommutative (x::Int) y) == True)

-- 4

mulAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
mulAssociative x y z = x * (y * z) == (x * y) * z

mulCommutative :: (Eq a, Num a) => a -> a -> Bool
mulCommutative x y = x * y == y * x

ex4 :: IO ()
ex4 = hspec $ do
  describe "* associativity" $ do
    it "parenthesization doesn't matter" $ do
      property $ (\x y z -> (mulAssociative (x::Int) y z) == True)
  describe "* commutative" $ do
    it "order doesn't matter" $ do
      property $ (\x y -> (mulCommutative (x::Int) y) == True)

-- 5.

quotRemProof :: Integral a => a -> a -> Bool
quotRemProof x y = (quot x y) * y + (rem x y) == x 

divModProof :: Integral a => a -> a -> Bool
divModProof x y = (div x y) * y + (mod x y) == x

-- QCh
quotRemProp :: Integer -> NonZero Integer -> Bool
quotRemProp x (NonZero y) = quotRemProof x y

divModProp :: Integer -> NonZero Integer -> Bool
divModProp x (NonZero y) = divModProof x y

ex5 :: IO ()
ex5 = hspec $ do
  describe "quot rem law" $ do
    it "should comply with the law" $ do
      property $ quotRemProp
  describe "div mod law" $ do
    it "should comply with the law" $ do
      property $ divModProp

-- 6.

prop_powerAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_powerAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)


prop_powerCommutative :: (Eq a, Integral a) => a -> a -> Bool
prop_powerCommutative x y = x ^ y == y ^ x

qc_powerAssociative :: IO ()
qc_powerAssociative = do
  quickCheck (prop_powerAssociative :: Int -> Int -> Int -> Bool)

qc_powerCommutative :: IO ()
qc_powerCommutative = do
  quickCheck (prop_powerCommutative :: Int -> Int -> Bool)
s

-- 7.

listReverseProp :: [Int] -> Bool
listReverseProp xs = (reverse . reverse $ xs) == xs

ex7 :: IO ()
ex7 = hspec $ do
  describe "list" $ do
    it "reversing xs twice equals xs" $ do
      property $ listReverseProp

-- 8.

dollarProp :: (Eq b) => (a -> b) -> a -> Bool
dollarProp f a = (f $ a) == f a

dotProp :: (Eq c) => (a -> b) -> (b -> c) -> a -> Bool
dotProp g f x = (f . g $ x) == f (g x)

ex8 :: IO ()
ex8 = hspec $ do
  describe "$" $ do
    it "f $ x and f x are the same" $ do
      property $ \x -> dollarProp (+1) (x::Int)
  describe "f . g $ x and f (g x) are the same" $ do
    it ". and function application are the same" $ do
      property $ \x -> dotProp (+20) (+3) (x::Int)

-- 9.

    it "flip (foldr (:)) == (++)" $ do
      property $ \xs ys -> flip (foldr (:)) xs ys === (++) (xs :: [Integer]) (ys :: [Integer])
    it "foldr (++) [] == concat" $ do
      property $ \xs -> foldr (++) [] xs === concat (xs :: [[Integer]])

-- 10.

fn10 :: (Positive Int) -> [a] -> Bool
fn10 (Positive n) xs = length (take n xs) == n

ex10 :: IO ()
ex10 = hspec $ do
  describe "fn10" $ do
    it "length (take n xs) and n are the same" $ do
      property $ \n -> fn10 n $ ([]::Int)

-- 11.

fn11 :: (Eq a, Read a, Show a) => a -> Bool
fn11 x = (read (show x)) == x

ex11 :: IO ()
ex11 = hspec $ do
  describe "fn11" $ do
    it "read show x and x are the same" $ do
      property $ \x -> fn11 (x::Char)

-- Failure

-- for a function
square :: Num a => a -> a
square x = x * x
-- Why does this property not hold? 
-- Examine the type of sqrt. 
squareIdentity :: Double -> Double
squareIdentity = square . sqrt

squapreProp :: Double -> Bool
squapreProp x = squareIdentity x == square x

exFailure :: IO ()
exFailure = verboseCheck squapreProp


-- Idempotence
twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

--

capitalizeWord :: String -> String 
capitalizeWord "" = ""
capitalizeWord (x:xs) = (Char.toUpper x) : xs

--

genLetter :: Gen Char
genLetter = choose ('a', 'z')

genWord :: Gen String
genWord = listOf1 genLetter -- listOf1 is non empty list

--

fnIdmp :: String -> Bool
fnIdmp x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

fnIdmp' :: (Ord a) => [a] -> Bool
fnIdmp' x = (List.sort x
  == twice List.sort x)
  &&
  (List.sort x
  == fourTimes List.sort x)

exIdempotence :: IO ()
exIdempotence = verboseCheck $ forAll genWord $ fnIdmp

exIdempotence' :: IO ()
exIdempotence' = verboseCheck $ forAll genWord $ fnIdmp'



-- Make a Gen random generator for the datatype

data Fool 
  = Fulse
  | Frue
  deriving (Eq, Show)

-- 1.
foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

-- 2.
foolGen' :: Gen Fool
foolGen' 
  = frequency 
  [ (7, return Fulse) -- round (2/3 * 10) = 7
  , (4, return Frue)] -- round (1/3 * 10) = 3
