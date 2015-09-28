{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Data.Function (on)

--------------------------------------------------------------------------------

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = ((+) `on` eval) x y
eval (Mul x y) = ((*) `on` eval) x y
