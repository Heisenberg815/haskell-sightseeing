{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized
import Buffer
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Ex 1

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single x _)   = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 Empty = jl1
(+++) Empty jl2 = jl2
(+++) jl1 jl2   = Append x jl1 jl2
  where
    x = mappend (tag jl1) (tag jl2)

-- Ex 2

-- indexJ
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ 
  | i < 0      = Nothing
indexJ _ Empty = Nothing 
indexJ i (Single _ y) 
  | i == 0     = Just y
  | otherwise  = Nothing
indexJ i (Append x jl1 jl2)
  | i >= s     = Nothing
  | i < s1     = indexJ i jl1
  | otherwise  = indexJ (i - s1) jl2
  where
    s  = getSize $ size $ x
    s1 = getSize $ size $ tag jl1

-- These 2 functions are useful to test indexJ
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0     = Just x
(_:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- dropJ
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl
  | i <= 0           = jl
dropJ _ Empty        = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append x jl1 jl2)
  | i >= s           = Empty
  | i < s1           = (dropJ i jl1) +++ jl2
  | otherwise        = dropJ (i - s1) jl2
  where
    s  = getSize $ size $ x
    s1 = getSize $ size $ tag jl1

-- takeJ
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _
  | i <= 0                = Empty
takeJ _ Empty             = Empty
takeJ _ jl @ (Single _ _) = jl
takeJ i jl @ (Append x jl1 jl2)
  | i >= s                = jl
  | i < s1                = takeJ i jl1
  | otherwise             = jl1 +++ (takeJ (i - s1) jl2)
  where
    s  = getSize $ size $ x
    s1 = getSize $ size $ tag jl1

-- Ex4

stringToJoinList :: String -> JoinList (Score, Size) String
stringToJoinList s = Single (scoreString s, Size 1) s

-- Note: only implemented the methods necessary to compute the score
-- That means that v, n, p, e commands wont work with this buffer
instance Buffer (JoinList (Score, Size) String) where
  fromString = foldr (+++) Empty . map stringToJoinList . words
  value      = getScore . fst . tag
