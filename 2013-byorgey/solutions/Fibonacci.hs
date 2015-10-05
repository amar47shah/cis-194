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
streamToList (Cons h t) = h : streamToList t

--------------------------------------------------------------------------------

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat e = Cons e $ streamRepeat e

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons h t) = Cons (f h) $ streamMap f t

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f h = Cons h . streamFromSeed f $ f h

--------------------------------------------------------------------------------

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed succ 0

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons h t) s = Cons h $ streamInterleave s t

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) $ streamMap succ ruler

--------------------------------------------------------------------------------

-- Exercise 6

x :: Stream Integer
x = Cons 0 . Cons 1 $ streamRepeat 0
