module Unfoldds where

myIterate :: (a -> a) -> a -> [a] 
myIterate f x = x : myIterate f (f x)

--

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
myUnfoldr f b = 
  let
    res = f b
  in
    case res of
      Just (a', b') -> a' : myUnfoldr f b'
      Nothing -> []
-- Break at 4

fn :: Int -> Maybe (Int, Int)
fn b
  | b < 5 = Just (b, b+1)
  | otherwise = Nothing

-- Infinite
fn2 :: Int -> Maybe (Int, Int)
fn2 b = Just (b, b+1)


--

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x
