{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized

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
(+++) jl1 jl2 = Append x jl1 jl2
  where
    x = mappend (tag jl1) (tag jl2)

-- Ex 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ 
  | i < 0 = Nothing
indexJ _ Empty = Nothing 
indexJ i (Single _ y) 
  | i == 0 = Just y
  | otherwise = Nothing
indexJ i (Append x jl1 jl2)
  | i >= s = Nothing
  | i < s1 = indexJ i jl1
  | otherwise = indexJ (i - s1) jl2
  where
    s  = getSize $ size $ x
    s1 = getSize $ size $ tag jl1
