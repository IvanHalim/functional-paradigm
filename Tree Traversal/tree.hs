module Main where

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))

-- | An example binary tree, which will be used in tests.
t1 :: Tree Int
t1 = Node 1 (Node 2 (Node 3 (leaf 4) (leaf 5))
                    (leaf 6))
            (Node 7 (leaf 8) (leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree Int
t2 = Node 6 (Node 2 (leaf 1) (Node 4 (leaf 3) (leaf 5)))
            (Node 8 (leaf 7) (leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (leaf 3)
--   Just 3
--
--   >>> leftmost (Node 5 (leaf 6) (leaf 7))
--   Just 6
--
--   >>> leftmost t1
--   Just 4
--
--   >>> leftmost t2
--   Just 1
--
--   >>> leftmost ex
--   Just 2
--
leftmost :: Tree a -> Maybe a
leftmost End = Nothing
leftmost (Node x left right) =
  case leftmost left of
    Just a  -> Just a
    Nothing -> Just x


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (leaf 3)
--   Just 3
--
--   >>> rightmost (Node 5 (leaf 6) (leaf 7))
--   Just 7
--
--   >>> rightmost t1
--   Just 9
--
--   >>> rightmost t2
--   Just 9
--
--   >>> rightmost ex
--   Just 8
--
rightmost :: Tree a -> Maybe a
rightmost End = Nothing
rightmost (Node x left right) =
  case rightmost right of
    Just a  -> Just a
    Nothing -> Just x


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (leaf 3)
--   Just 3
--
--   >>> maxInt (Node 5 (leaf 4) (leaf 2))
--   Just 5
--
--   >>> maxInt (Node 5 (leaf 7) (leaf 2))
--   Just 7
--
--   >>> maxInt t1
--   Just 9
--
--   >>> maxInt t2
--   Just 9
--
--   >>> maxInt ex
--   Just 8
--
maxInt :: Ord a => Tree a -> Maybe a
maxInt End = Nothing
maxInt (Node x left right) =
  case maxInt left of
    Just a  -> case maxInt right of
      Just b  -> Just (max (max a b) x)
      Nothing -> Just (max a x)
    Nothing -> case maxInt right of
      Just b  -> Just (max b x)
      Nothing -> Just x


-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (leaf 3)
--   Just 3
--
--   >>> minInt (Node 2 (leaf 5) (leaf 4))
--   Just 2
--
--   >>> minInt (Node 5 (leaf 4) (leaf 7))
--   Just 4
--
--   >>> minInt t1
--   Just 1
--
--   >>> minInt t2
--   Just 1
--
--   >>> minInt ex
--   Just 2
--
minInt :: Ord a => Tree a -> Maybe a
minInt End = Nothing
minInt (Node x left right) =
  case minInt left of
    Just a  -> case minInt right of
      Just b  -> Just (min (min a b) x)
      Nothing -> Just (min a x)
    Nothing -> case minInt right of
      Just b  -> Just (min b x)
      Nothing -> Just x


-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (leaf 3)
--   3
--
--   >>> sumInts (Node 2 (leaf 5) (leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--
--   >>> sumInts ex
--   35
--
sumInts :: Num a => Tree a -> a
sumInts End = 0
sumInts (Node x left right) = x + sumInts left + sumInts right


-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (leaf 6) (leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--
--   >>> preorder ex
--   [4,3,2,7,5,6,8]
--
preorder :: Tree a -> [a]
preorder End = []
preorder (Node x left right) = x : preorder left ++ preorder right


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (leaf 6) (leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> inorder ex
--   [2,3,4,5,6,7,8]
--
inorder :: Tree a -> [a]
inorder End = []
inorder (Node x left right) = inorder left ++ x : inorder right


-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (leaf 3)
--   True
--
--   >>> isBST (Node 5 (leaf 6) (leaf 7))
--   False
--
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--
--   >>> isBST ex
--   True
--
isBST :: Ord a => Tree a -> Bool
isBST End = True
isBST (Node x left right) = case rightmost left of
  Just a  -> case leftmost right of
    Just b  -> x >= a && x <= b && isBST left && isBST right
    Nothing -> x >= a && isBST left
  Nothing -> case leftmost right of
    Just b  -> x <= b && isBST right
    Nothing -> True


-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (leaf 2) (leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (leaf 2) (leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--
--   >>> inBST 10 ex
--   False
--
--   >>> inBST 8 ex
--   True
--
inBST :: Eq a => a -> Tree a -> Bool
inBST _ End = False
inBST x (Node y left right) =
  x == y || inBST x left || inBST x right


-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ End          = End
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)


-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
valueAt :: Path -> Tree a -> Maybe a
valueAt _     End          = Nothing
valueAt []    (Node x _ _) = Just x
valueAt (h:t) (Node _ l r)
  | h == L = valueAt t l
  | h == R = valueAt t r


-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo _ End = Nothing
pathTo y (Node x l r)
  | y == x    = Just []
  | otherwise = case pathTo y l of
      Just lpath -> Just (L:lpath)
      Nothing    -> case pathTo y r of
          Just rpath -> Just (R:rpath)
          Nothing    -> Nothing

main :: IO ()
main = return ()