-- Mood swing (pg.135)

-- Given:
data Mood = Blah | Woot deriving Show
-- 1. Type constructor is `Mood`
-- 2. Possible values are Data constructors `Blah` and `Woot`
-- 3. 
-- changeMood :: Mood -> Woot is wrong because function type declarations are declared with Type constructors not Data constructors.
changeMood1 :: Mood -> Mood -- is the right type declaration
changeMood1 = undefined
-- 4.
changeMood2 Blah = Woot
changeMood2 _ = Blah


-- Find the Mistakes (pg.153)
findMistakes_x = 5
findMistakes_1 = not True && True
findMistakes_2 = not (findMistakes_x == 6)
findMistakes_3 = (1 * 2) > 5
findMistakes_4 = ["Merry"] > ["Happy"]
findMistakes_5 = ['1','2','3'] ++ "look at me!"


-- Chapter exercises (pg.165)
awesome = ["Papuchon", "curry", ":"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1.
len :: [a] -> Int
len list = length list

-- 2.
-- a)
pg165a = length [1,2,3,4,5] == 5
-- b)
pg165b = length [(1,2),(2,3),(3,4)] == 3
-- c)
pg165c = length allAwesome == 2
-- d)
pg165d = length (concat allAwesome) == 5

-- 3.
-- Prelude> 6 / 3 --> is ok
-- Prelude> 6 / length [1,2,3] --> is not ok because `length` returns `Int`, not `Fractional`

-- 4.
pg166_4 = div 6 (length [1,2,3]) -- Integral division works

-- 5.
pg166_5 = 2 + 3 == 5 -- Type is Bool

-- 6.
pg166_6_x = 5
pg166_6_1 = pg166_6_x + 3 == 5 -- Type is Bool, result False

-- 7.
-- lenght allAwesome == 2 -- Ok, evaluates to True
-- length [1,'a',3,'b'] -- Ko, lists can't contain heterogenous types
-- length allAwesome + length awesome -- Ok, the sum of the two lengths

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome str =
  str == reverse str

-- 9.
myAbs :: Integer -> Integer
myAbs n = if n < 0 then (negate n) else n

-- 10.
f1 :: (a,b) -> (c,d) -> ( (b,d), (a,c) )
f1 t1 t2 = ( (snd t1, snd t2), (fst t1, fst t2) )


-- Correcting syntax (pg.168)

-- 1
x = (+)
f2 :: String -> Int
f2 xs = w `x` 1
  where w = length xs

-- 2
identity = \x -> x

-- 3
head2 = \(x:xs) -> x

-- 4
tupleFst (a,b) = a


-- Match the funciton names to their types (pg.169)
-- 1 c)
-- 2 b)
-- 3 a)
-- 4 d)
