{-# OPTIONS_GHC -Wall #-}

-- Ex1
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0     = []
  | otherwise = (mod n 10) : toDigitsRev(div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Ex2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []        = []
doubleEveryOther (x:xs) 
  | length xs `mod` 2 == 0 = x:(doubleEveryOther xs)
  | otherwise              = (2 * x):(doubleEveryOther xs)

-- Ex3
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits xs 

-- Ex4
validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0 

-- Ex5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c
  | n < 0     = []
  | otherwise = concat [(hanoi (n - 1) a c b), [(a, b)], (hanoi(n - 1) c b a)]
