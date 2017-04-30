module Main where

import JoinList
import Editor
import Scrabble
import Sized

jlb :: JoinList (Score, Size) String
jlb = Empty

main = runEditor editor jlb
