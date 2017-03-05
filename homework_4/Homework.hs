{-# OPTIONS_GHC -Wall #-}
import Data.List

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

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf                         = Node 0 Leaf a Leaf
insertTree a (Node 0 Leaf b Leaf)         = Node 1 (Node 0 Leaf a Leaf) b Leaf
insertTree a (Node h ltree b Leaf)        = Node h ltree b (Node 0 Leaf a Leaf)
insertTree a (Node h Leaf b rtree)        = Node h (Node 0 Leaf a Leaf) b rtree
insertTree a (Node h ltree @ (Node lh _ _ _) b rtree @ (Node rh _ _ _))
  | lh < rh                           = Node h newltree b rtree
  | lh > rh                           = Node h ltree b newrtree
  | lh == rh && not (isPerfect ltree) = Node h newltree b rtree
  | lh == rh && not (isPerfect rtree) = Node h ltree b newrtree
  | otherwise                         = Node (1 + h) newltree b rtree
  where
    newltree = insertTree a ltree
    newrtree = insertTree a rtree

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

-- Ex 3
-- 1)
xor :: [Bool] -> Bool
xor = foldr (/=) False

-- 2)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> (f x) : l) []

-- Ex 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

f1 :: Integer -> ([Integer], [(Integer, Integer)])
f1 n = ([1..n], cartProd [1..n] [1..n])

f2 :: ([Integer], [(Integer, Integer)]) -> ([Integer], [Integer])
f2 c = (fst c, map (\(i, j) -> i + j + 2 * i * j) (snd c))

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1) . (*2)) . (uncurry (\\)) . f2 . f1
