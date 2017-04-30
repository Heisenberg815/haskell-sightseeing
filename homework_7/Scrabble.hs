{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble where

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

-- Ex3
score :: Char -> Score
score c
  | elem c ['a', 'e', 'i', 'l', 'n', 'o', 'r', 's', 't', 'u',
            'A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U'] = Score 1
  | elem c ['d', 'g', 'D', 'G']                               = Score 2
  | elem c ['b', 'c', 'm', 'p', 'B', 'C', 'M', 'P']           = Score 3
  | elem c ['f', 'h', 'v', 'w', 'y', 'F', 'H', 'V', 'W', 'Y'] = Score 4
  | elem c ['k', 'K']                                         = Score 5
  | elem c ['j', 'x', 'J', 'X']                               = Score 8
  | elem c ['q', 'z', 'Q', 'Z']                               = Score 10
  | otherwise                                                 = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score
