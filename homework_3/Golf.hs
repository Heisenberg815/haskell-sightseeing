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
  | otherwise      = (localMaxima (y:z:xs))
localMaxima _      = []
