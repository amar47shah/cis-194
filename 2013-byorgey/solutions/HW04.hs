{-# OPTIONS_GHC -Wall #-}
module HW04 where

--------------------------------------------------------------------------------

-- Exercise 1

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where f n | even n    = n `div` 2
            | otherwise = 3 * n + 1

fun1 :: [Integer] -> Integer
fun1 []      = 1
fun1 (x:xs)
 | even x    = (x - 2) * fun1 xs
 | otherwise =           fun1 xs

-- WARNING: fun2 and fun2' produce infinite loops for input < 1
fun2 :: Integer -> Integer
fun2 1        = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise =     fun2 (3 * n + 1)

--------------------------------------------------------------------------------

-- Exercise 2

-- | Binary tree with memoized heights.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- | Generates a balanced binary tree.
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- | Inserts into a balanced binary tree,
-- | retaining balanced property and updating heights.
insert :: a -> Tree a -> Tree a
insert x  Leaf                            = singleton x
insert x (Node _ Leaf y Leaf)             = Node 1 (singleton x) y Leaf
insert x (Node _ l@(Node _ _ _ _) y Leaf) = Node 1 l y (singleton x)
insert x (Node _ Leaf y r@(Node _ _ _ _)) = Node 1 (singleton x) y r
insert x (Node h l@(Node hl _ _ _) y r@(Node hr _ _ _))
  | hl > hr    = Node h         l  y r'
  | hl < hr    = Node h         l' y r
  | hasSpace l = Node h         l' y r
  | hasSpace r = Node h         l  y r'
  | otherwise  = Node (hl' + 1) l' y r
    where l'@(Node hl' _ _ _) = insert x l
          r'                  = insert x r

-- | Generates a binary tree with one element.
singleton :: a -> Tree a
singleton x = Node 0 Leaf x Leaf

-- | Determines whether a new node can be inserted without increasing height.
hasSpace :: Tree a -> Bool
hasSpace  Leaf                          = False
hasSpace (Node _ Leaf _ (Node _ _ _ _)) = True
hasSpace (Node _ (Node _ _ _ _) _ Leaf) = True
hasSpace (Node _ l _ r)                 = hasSpace l || hasSpace r
