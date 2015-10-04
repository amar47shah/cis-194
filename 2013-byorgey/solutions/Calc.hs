{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as S

import Control.Monad (liftM)
import Data.Function (on)

--------------------------------------------------------------------------------

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = ((+) `on` eval) x y
eval (Mul x y) = ((*) `on` eval) x y

--------------------------------------------------------------------------------`

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = liftM eval . parseExp Lit Add Mul

--------------------------------------------------------------------------------`

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

--------------------------------------------------------------------------------`

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

--------------------------------------------------------------------------------

-- Exercise 5

instance Expr S.Program where
  lit = (: []) . S.PushI
  add = combine S.Add
  mul = combine S.Mul

combine :: S.StackExp -> S.Program -> S.Program -> S.Program
combine op p q = concat [pushResult p, pushResult q, [op]]

pushResult :: S.Program -> S.Program
pushResult = programFromResult . S.stackVM

-- Raises error
programFromResult :: Either String S.StackVal -> S.Program
programFromResult (Left message) = error message
programFromResult (Right value)  = programFrom value

programFrom :: S.StackVal -> S.Program
programFrom (S.IVal i) = [S.PushI i]
programFrom (S.BVal b) = [S.PushB b]
programFrom _          = []
