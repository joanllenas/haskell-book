module BinaryTree where

data BinaryTree a 
  = Leaf 
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left y right)
  | x == y = Node left y right
  | x < y = Node(insert' x left) y right 
  | otherwise = Node left y (insert' x right)

--- Write map for BinaryTree

mapTree :: (a -> b)
  -> BinaryTree a
  -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left x right) 
  = Node (mapTree f left) (f x) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 
  1
  (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected 
    then print "yup OK!"
    else error "test failed!"



--- Convert binary trees to lists

preorder :: BinaryTree a -> [a] 
preorder Leaf = []
preorder (Node left x right) 
  = x 
  : (preorder left) 
  ++ (preorder right)


inorder :: BinaryTree a -> [a] 
inorder Leaf = []
inorder (Node left x right) 
  = (inorder left) 
  ++ [x] 
  ++ (inorder right)

postorder :: BinaryTree a -> [a] 
postorder Leaf = []
postorder (Node left x right) 
  = (postorder left) 
  ++ (postorder right) 
  ++ [x]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf) 
  2
  (Node Leaf 3 Leaf)


testPreorder =
  if preorder testTree == [2, 1, 3] 
  then "Preorder fine!" 
  else "Preorder bad."

testInorder =
  if inorder testTree == [1, 2, 3] 
  then "Inorder fine!" 
  else "Inorder bad."

testPostorder =
  if postorder testTree == [1, 3, 2] 
  then "Postorder fine!" 
  else "Postorder bad"

listOkay =
  testPreorder 
  ++ ", " ++
  testInorder 
  ++ ", " ++
  testPostorder



--- Write foldr for BinaryTree

-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = 
  foldTree f (foldTree f (f a b) left) right

foldOkay = 
  if foldTree (+) 0 testTree == 6 
  then "fold fine!"
  else "fold bad"