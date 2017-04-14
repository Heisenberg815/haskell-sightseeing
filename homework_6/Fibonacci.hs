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

-- Ex3

data Stream a = a :< (Stream a)
  deriving (Eq)

streamToList :: Stream a -> [a]
streamToList (x:<s) = x:(streamToList s)

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

-- Ex 4

streamRepeat :: a -> Stream a
streamRepeat x = x:<(streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x:<s) = (f x):<(streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x:<(streamFromSeed f (f x))

-- Ex 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Didn't find smart way to implement ruler
greatest2Divider :: Integer -> Integer
greatest2Divider n
  | n <= 0 = 0
  | n `mod` 2 == 1 = 0
  | otherwise      = 1 + greatest2Divider(n `div` 2)

ruler :: Stream Integer
ruler = streamMap greatest2Divider nats
