{-# OPTIONS_GHC -Wall #-}

-- Ex1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Ex2

fibs2 :: [Integer]
fibs2 = 0 : 1: zipWith (+) fibs2 (tail(fibs2))
