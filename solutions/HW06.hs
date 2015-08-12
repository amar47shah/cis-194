{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 20 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x . sIterate f $ f x

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons h t) s = Cons h $ sInterleave s t

sTake :: Int -> Stream a -> [a]
sTake     = (. streamToList) . take
   -- n   = take n . streamToList
   -- n s = take n (streamToList s)

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate succ 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) . (succ <$>) $ ruler

-- Verify that the pattern works for 2^17 = 131072 terms
testRuler :: Bool
testRuler =
      -- each x in [0..n] first occurs at index 2^x - 1
      map (`elemIndex` sTake (2^n) ruler) [0..n] ==
      map (\x -> Just (2^x-1)) [0..n]
    &&
      -- each x in [0..n-1] appears (n-x-1) times in the first 2^n - 1 terms
      map (\x -> length . filter (== x) $ sTake (2^n-1) ruler) [0..n-1] ==
      reverse (map (2^) [0..n-1])
    where n = 17 -- larger than 17 is too slow

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand s = Cons g $ rand g
  where g = (1103515245 * s + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = undefined

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
