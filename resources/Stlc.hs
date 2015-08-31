{-# LANGUAGE GADTs                                  #-}
{-# LANGUAGE TypeFamilies                           #-}
{-# LANGUAGE TypeOperators                          #-}
module Stlc where

import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Type.Equality

data Type :: * -> * where
  TInt      :: Type Int
  TBool     :: Type Bool
  TArrow    :: Type a -> Type b -> Type (a -> b)

instance Show (Type a) where
    show TInt  = "Int"
    show TBool = "Bool"
    show (TArrow a@TArrow{} b) = "(" ++ show a ++ ") -> " ++ show b
    show (TArrow a b)          = show a ++ " -> " ++ show b

class TypeOf a where
    typeOf :: a -> Type a

instance TypeOf Int where
    typeOf _ = TInt

instance TypeOf Bool where
    typeOf _ = TBool

data Bop :: * -> * -> * where
  Add :: Bop Int Int
  Sub :: Bop Int Int
  Eq  :: Bop Int Bool
  Lt  :: Bop Int Bool
  Gt  :: Bop Int Bool
  And :: Bop Bool Bool
  Or  :: Bop Bool Bool

instance Show (Bop a b) where
    show Add = "+"
    show Sub = "-"
    show Eq  = "="
    show Lt  = "<"
    show Gt  = ">"
    show And = "&"
    show Or  = "|"

data Expr :: * -> * where
  Lit     :: (TypeOf a, Show a) => a -> Expr a
  Var     :: String -> Type a -> Expr a
  Lambda  :: String -> Type a -> Expr b -> Expr (a -> b)
  App     :: Expr (a -> b) -> Expr a -> Expr b
  Bop     :: Bop a b -> Expr a -> Expr a -> Expr b
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a
  Lift    :: a -> Type a -> Expr a

instance Show (Expr t) where
    show (Lit x)        = show x
    show (Var v _)      = v
    show (Lambda x t e) = concat ["\x03bb", x, ":", show t, ". ", show e]
    show (App e1 e2)    = s1 ++ " " ++ s2
        where s1 = case e1 of
                     Lambda{} -> "(" ++ show e1 ++ ")"
                     _        -> show e1
              s2 = case e2 of
                     Var{} -> show e2
                     Lit{} -> show e2
                     _     -> "(" ++ show e2 ++ ")"
    show (Bop b e1 e2)  = intercalate " " [show e1, show b, show e2]
    show (If b e1 e2)   = intercalate " "
                          ["if", show b, "then", show e1, "else", show e2]
    show (Lift _ t)     = "<<" ++ show t ++ ">>"


onePlusTwo :: Expr Int
onePlusTwo = Bop Add (Lit 1) (Lit 2)


plusOne :: Expr (Int -> Int)
plusOne = Lambda "x" TInt $ Bop Add (Var "x" TInt) (Lit 1)

garbage :: Expr (Int -> Bool)
garbage = Lambda "x" TInt $ Bop And (Var "x" TBool) (Lit True)


class TypeOfExpr a where
    typeOfExpr :: Expr a -> Type a

instance TypeOfExpr Int where
    typeOfExpr _ = TInt

instance TypeOfExpr Bool where
    typeOfExpr _ = TBool

typeOfBop :: Bop a b -> Type (a -> a -> b)                                 
typeOfBop bop = TArrow inTy $ TArrow inTy outTy                            
    where inTy  = case bop of { Add -> TInt  ; Sub -> TInt  ; Eq  -> TInt  
                              ; Lt  -> TInt  ; Gt  -> TInt  ; And -> TBool 
                              ; Or  -> TBool                               
                              }                                            
          outTy = case bop of { Add -> TInt  ; Sub -> TInt  ; Eq  -> TBool 
                              ; Lt  -> TBool ; Gt  -> TBool ; And -> TBool 
                              ; Or  -> TBool                               
                              }                                            

instance TypeOfExpr b => TypeOfExpr (a -> b) where
    typeOfExpr (Var _ t)      = t
    typeOfExpr (Lambda _ t e) = TArrow t $ typeOfExpr e
    typeOfExpr (App e1 _)     =
        case typeOfExpr e1 of
          TArrow _ t2 -> t2
    typeOfExpr (Bop b _ _)    =
        case typeOfBop b of
          TArrow _ (TArrow _ t3) -> t3
    typeOfExpr (If _ e1 _)    = typeOfExpr e1
    typeOfExpr (Lift _ t)     = t

eq :: Type a -> Type b -> Maybe (a :~: b)
eq TInt  TInt  = Just Refl
eq TBool TBool = Just Refl
eq (TArrow u1 u2) (TArrow v1 v2) = do
  Refl <- eq u1 v1
  Refl <- eq u2 v2
  return Refl
eq _     _     = Nothing

evalBop :: Bop a b -> a -> a -> b
evalBop Add = (+)
evalBop Sub = (-)
evalBop Eq  = (==)
evalBop Lt  = (<)
evalBop Gt  = (>)
evalBop And = (&&)
evalBop Or  = (||)

eval :: Expr t -> t
eval (Lit x)        = x
eval (Var _ _)      = error "free variables"
eval (Lambda x t e) = \y -> eval $ subst x y t e
eval (App e1 e2)    = (eval e1) (eval e2)
eval (Bop b e1 e2)  = (evalBop b) (eval e1) (eval e2)
eval (If c e1 e2)   | eval c    = eval e1
                    | otherwise = eval e2
eval (Lift x _)     = x

subst :: String -> u -> Type u -> Expr t -> Expr t
subst _ _ _ (Lit b) = Lit b
subst x v u (Var y t)
    | x == y    =
        case eq u t of
          Just Refl -> Lift v t
          Nothing   -> error "can't substitute"
    | otherwise = Var y t
subst x v u (Bop b e1 e2) = Bop b
                            (subst x v u e1)
                            (subst x v u e2)
subst x v u (If e1 e2 e3) = If (subst x v u e1)
                               (subst x v u e2)
                               (subst x v u e3)
subst x v u (Lambda y t e) | x == y    = Lambda y t e
                           | otherwise = Lambda y t (subst x v u e)
subst x v u (App e1 e2) = App (subst x v u e1) (subst x v u e2)
subst _ _ _ (Lift x t)  = Lift x t


abs :: Expr (Int -> Int)
abs = Lambda "x" TInt $ If (Bop Lt (Var "x" TInt) (Lit 0))
               {- then -}  (Bop Sub (Lit 0) (Var "x" TInt))
               {- else -}  (Var "x" TInt)
