{- CIS 194 HW 11
   due Monday, 8 April
-}
{-# OPTIONS_GHC -Wall #-}

module SExpr where

import AParser
import Control.Applicative hiding ((*>))
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Prelude hiding ((*>), sequenceA)

------------------------------------------------------------
-- End-of-Lesson Questions
------------------------------------------------------------

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 $ flip const

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA = (sequenceA .) . map

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
sequenceA _      = pure []

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = (sequenceA .) . replicate

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
