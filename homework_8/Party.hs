{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Employee

-- Ex 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL employees fun) = GL (e:employees) (fun + (empFun e))

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 @ (GL _ f1) g2 @ (GL _ f2)
  | f1 > f2   = g1
  | otherwise = g2

-- Ex 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x subtrees) = f x (map (treeFold f) subtrees)

-- Ex3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e l = (glCons e (mconcat (map snd l)), mconcat (map fst l))

-- Ex4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel
