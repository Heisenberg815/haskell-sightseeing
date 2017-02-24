{-# OPTIONS_GHC -Wall #-}

-- Ex 1
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . (filter even)

---
syracuse :: Integer->Integer
syracuse x 
  | even x = x `div` 2
  | otherwise = 3 * x + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate syracuse 
