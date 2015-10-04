{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as S

import Control.Applicative ((<$>), liftA2)
import Data.Function (on)
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = ((+) `on` eval) x y
eval (Mul x y) = ((*) `on` eval) x y

--------------------------------------------------------------------------------`

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . parseExp Lit Add Mul

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
  add = combineWith S.Add
  mul = combineWith S.Mul

combineWith :: S.StackExp -> S.Program -> S.Program -> S.Program
combineWith op p q = p ++ q ++ [op]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

--------------------------------------------------------------------------------

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = VVar

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add = liftA2 $ liftA2 (+)
  mul = liftA2 $ liftA2 (*)

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs e = e $ M.fromList vs
