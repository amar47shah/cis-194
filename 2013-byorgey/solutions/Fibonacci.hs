{-# OPTIONS_GHC -Wall #-}
module Fibonacci where

import Data.List (intersperse)

--------------------------------------------------------------------------------

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--------------------------------------------------------------------------------

-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

--------------------------------------------------------------------------------

-- Exercise 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show =
    (++ "...") . concat . (intersperse ",") . map show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

--------------------------------------------------------------------------------

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x . streamFromSeed f $ f x
