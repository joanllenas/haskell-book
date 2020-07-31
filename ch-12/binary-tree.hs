module BinaryTree where

data BinaryTree a 
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)



myUnfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b 
myUnfoldTree f x =
  case f x of
    Just (a,b,a') -> Node (myUnfoldTree f a) b (myUnfoldTree f a')
    Nothing -> Leaf

-- test fn
testFn :: Int -> Maybe (Int,String,Int)
testFn a = 
  if a > 5 then Nothing
  else Just (a+1, show a, a+2)
{- 
myUnfoldTree testFn 0
-- Node (Node (Node (Node (Node (Node Leaf "5" Leaf) "4" Leaf) "3" (Node Leaf "5" Leaf)) "2" (Node (Node Leaf "5" Leaf) "4" Leaf)) "1" (Node (Node (Node Leaf "5" Leaf) "4" Leaf) "3" (Node Leaf "5" Leaf))) "0" (Node (Node (Node (Node Leaf "5" Leaf) "4" Leaf) "3" (Node Leaf "5" Leaf)) "2" (Node (Node Leaf "5" Leaf) "4" Leaf))
-}

--

treeBuild :: Integer -> BinaryTree Integer
treeBuild n
  | n < 0     = Leaf
  | otherwise = myUnfoldTree f 0
    where
      f k
        | k == n    = Nothing
        | otherwise = Just (k+1, k, k+1)