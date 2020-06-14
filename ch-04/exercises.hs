fn :: Int -> Bool
fn x = not (x == 6)

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness 
    then putStrLn "eyy"
  else
    putStrLn "pshh"
  where
    cool v = v == "down"


isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome xs = reverse xs == xs

isPalindrome2 :: (Eq a) => [a] -> Bool 
isPalindrome2 xs =
  if 
    len > 1 
  then 
    compareTails xs 0 (len-1) True
  else 
    False
  where
    len = length xs
    compareTails :: (Eq a) => [a] -> Int -> Int -> Bool -> Bool
    compareTails xs2 indexL indexR acc =
      let
        tailLettersMatch = (xs2 !! indexL) == (xs2 !! indexR)
        acc2 = acc && tailLettersMatch
      in
        if 
          ((indexL + 1) == (indexR -1) || (indexL + 1) > indexR )
        then 
          acc2
        else
          compareTails xs2 (indexL+1) (indexR-1) acc2

myAbs :: Integer -> Integer 
myAbs x = if x < 0 then negate x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c)) 
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))