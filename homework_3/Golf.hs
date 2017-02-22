{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Ex 1
takeEveryNelemFromEnd :: Int -> [a] -> [a]
takeEveryNelemFromEnd _ [] = []
takeEveryNelemFromEnd n l @ (x:xs)
  | n < 1                  = []
  | length l `mod` n == 0  = x:(takeEveryNelemFromEnd n xs)
  | otherwise              = takeEveryNelemFromEnd n xs

takeEveryNelemFromBeginning :: Int -> [a] -> [a]
takeEveryNelemFromBeginning n l = reverse(takeEveryNelemFromEnd n l')
  where l' = reverse l

skips :: [a] -> [[a]]
skips l = map z x
  where
    z n = takeEveryNelemFromBeginning n l
    x   = [1..length(l)]

-- Ex 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y:(localMaxima (z:xs))
  | otherwise      = localMaxima (y:z:xs)
localMaxima _      = []

-- Ex 3

-- This exercice is split into 2 parts:
-- 1: build findNbOccurencesFor0To9 which computes the nb of occurrences
-- of each the numbers 0..9 in the input list
-- 2: write nbOccToHisto, which builds the histogram from the nb of occurrences

findNbOccurrences :: Integer -> [Integer] -> Integer
findNbOccurrences _ [] = 0
findNbOccurrences n (x:xs)
  | x == n             = 1 + (findNbOccurrences n xs)
  | otherwise          = findNbOccurrences n xs

findNbOccurencesFor0To9 :: [Integer] -> [Integer]
findNbOccurencesFor0To9 l = map z [0..9]
  where z n               = findNbOccurrences n l

---
asteriskIfEqual :: Integer -> Integer -> Char
asteriskIfEqual x y
  | x == y    = '*'
  | otherwise = ' '

asteriskIfMax :: [Integer] -> String
asteriskIfMax l = map z l
  where
    m   = maximum l
    z n = asteriskIfEqual m n

decrementMax :: Integer -> [Integer] -> [Integer]
decrementMax _ [] = []
decrementMax n (x:xs)
  | x == n        = (x - 1):(decrementMax n xs)
  | otherwise     = x:(decrementMax n xs)

nbOccToHisto :: [Integer] -> String
nbOccToHisto [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] = "==========\n0123456789\n"
nbOccToHisto l                              = asteriskIfMax l ++ "\n" ++
                                              nbOccToHisto (decrementMax m l)
  where m = maximum l

---

histogram :: [Integer] -> String
histogram l = nbOccToHisto (findNbOccurencesFor0To9 l)
