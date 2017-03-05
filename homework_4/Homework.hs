{-# OPTIONS_GHC -Wall #-}

-- Ex 1
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . (filter even)

---
syracuse :: Integer-> Integer
syracuse x 
  | even x    = x `div` 2
  | otherwise = 3 * x + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate syracuse 

-- Ex 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

nbNodes :: Tree a -> Integer
nbNodes Leaf                   = 0
nbNodes (Node _ ltree _ rtree) = 1 + (nbNodes ltree) + (nbNodes rtree)

isPerfect :: Tree a -> Bool
isPerfect Leaf               = True
isPerfect t @ (Node h _ _ _) = nbNodes t == 2 ^ (h + 1) - 1

insert :: a -> Tree a -> Tree a
insert a Leaf                         = Node 0 Leaf a Leaf
insert a (Node 0 Leaf b Leaf)         = Node 1 (Node 0 Leaf a Leaf) b Leaf
insert a (Node h ltree b Leaf)        = Node h ltree b (Node 0 Leaf a Leaf)
insert a (Node h Leaf b rtree)        = Node h (Node 0 Leaf a Leaf) b rtree
insert a (Node h ltree @ (Node lh _ _ _) b rtree @ (Node rh _ _ _))
  | lh < rh                           = Node h newltree b rtree
  | lh > rh                           = Node h ltree b newrtree
  | lh == rh && not (isPerfect ltree) = Node h newltree b rtree
  | lh == rh && not (isPerfect rtree) = Node h ltree b newrtree
  | otherwise                         = Node (1 + h) newltree b rtree
  where
    newltree = insert a ltree
    newrtree = insert a rtree

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
