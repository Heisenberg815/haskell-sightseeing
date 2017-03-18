{-# OPTIONS_GHC -Wall #-}
module Calc where
import ExprT
import Parser

-- Ex 1
eval :: ExprT -> Integer
eval (Lit x)     = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

-- Ex 2
evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe Nothing = Nothing
evalMaybe (Just e) = Just (eval e)

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . (parseExp Lit Add Mul)

-- Ex 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Ex 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = (MinMax)
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit a = Mod7 $ mod a 7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7

-- tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
