{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser (Parser, runParser, satisfy, char, posInt) where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

--------------------------------------------------------------------------------

-- Exercise 1

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

--------------------------------------------------------------------------------

-- Exercise 2

instance Functor Parser where
  fmap f p = Parser $ \s -> runParser p s >>= Just . first f

instance Applicative Parser where
  pure x = Parser $ Just . (,) x
  p1 <*> p2 = Parser $
    \s -> runParser p1 s >>= \(f, s') -> runParser p2 s' >>= Just . first f

--------------------------------------------------------------------------------

-- Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = ignore abParser

intPair :: Parser [Integer]
intPair = (:) <$> posInt <*> (const (:[]) <$> char ' ' <*> posInt)

--------------------------------------------------------------------------------

-- Exercise 4

instance Alternative Parser where
  empty = Parser $ const empty
  p1 <|> p2 = Parser $ (<|>) <$> runParser p1 <*> runParser p2

--------------------------------------------------------------------------------

ignore :: Parser a -> Parser ()
ignore = (const () <$>)

upperChar :: Parser Char
upperChar = satisfy isUpper

-- Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = ignore posInt <|> ignore upperChar
