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
