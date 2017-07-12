{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


------------------------------------------------------------
-- Ex 2
nbSoldiers :: Army -> Army -> (Int, Int)
nbSoldiers 0 _ = (0, 0)
nbSoldiers _ 0 = (0, 0)
nbSoldiers 1 _ = (0, 0)
nbSoldiers 2 1 = (1, 1)
nbSoldiers 2 _ = (1, 2)
nbSoldiers 3 1 = (2, 1)
nbSoldiers 3 _ = (2, 2)
nbSoldiers _ 1 = (3, 1)
nbSoldiers _ _ = (3, 2)

simulateDice :: Int -> Rand StdGen [DieValue]
simulateDice n = sequence (replicate n die)

nbCasualties :: [DieValue] -> [DieValue] -> (Int, Int)
nbCasualties [] _ = (0, 0)
nbCasualties _ [] = (0, 0)
nbCasualties (h1:t1) (h2:t2)
  | h1 > h2   = add (0, 1) (nbCasualties t1 t2)
  | otherwise = add (1, 0) (nbCasualties t1 t2)
    where add = \(x1, x2) (y1, y2) -> (x1 + y1, x2 + y2)

orderDV :: DieValue -> DieValue -> Ordering
orderDV d1 d2
  | i1 > i2   = GT
  | i1 == i2  = EQ
  | otherwise = LT
    where
      i1 = unDV d1
      i2 = unDV d2

subtractCasualties :: Battlefield -> (Int, Int) -> Battlefield
subtractCasualties (Battlefield x1 x2) (c1, c2) = Battlefield (x1 - c1) (x2 - c2)

battle :: Battlefield -> Rand StdGen Battlefield
battle b @ (Battlefield x1 x2) = subtractCasualties <$> return b <*> cs
  where
    (s1, s2) = nbSoldiers x1 x2
    dice1 = fmap (sortBy orderDV) (simulateDice s1)
    dice2 = fmap (sortBy orderDV) (simulateDice s2)
    cs = nbCasualties <$> dice1 <*> dice2

-- Ex 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b @ (Battlefield x y)
  | x < 2 || y < 1 = return b
  | otherwise      = battle b >>= invade

-- Ex 4
successProb :: Battlefield -> Rand StdGen Double
successProb b = fmap ratio $ sequence $ fmap (fmap attackersWin . invade) (replicate 1000 b)
  where
    attackersWin (Battlefield _ y) = if y == 0 then 1 else 0
    ratio :: [Int] -> Double
    ratio l = (fromIntegral (sum l)) / (fromIntegral (length l))
