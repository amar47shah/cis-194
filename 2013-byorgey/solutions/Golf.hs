{-# OPTIONS_GHC -Wall #-}
module Golf (histogram, localMaxima, skips) where

import Data.List (tails)

skips :: [a] -> [[a]]
-- Use scanr to get successively reduced values from right,
-- where right-most value is [] and subsequent values are cons-ed.
--
-- Use filter (not . null) to remove [] from list, although init
-- can safely do the same. 33 chars.
-- skips = filter (not . null) . scanr (:) []
--
-- with init instead: 21 chars.
-- skips = init . scanr (:) []
--
-- Data.List.tails provides the same function as scanr (:) []
-- 28 chars:
-- skips = filter (not . null) . tails
--
-- Using init, 16 chars:
skips = init . tails

localMaxima :: [Integer] -> [Integer]
-- Map (take 3) over tails to get sliding windows
-- Filter to ensure all windows have length 3.
-- Filter to get windows with local maxima
-- Map to isolate maxima.
-- 93 chars:
localMaxima = map (\[_,y,_] -> y) .
                filter (\[x,y,z] -> x < y && y > z) .
                  filter ((== 3) . length) .
                    map (take 3) .
                      tails

histogram :: [Integer] -> String
histogram xs = helper . counts $ map fromIntegral xs
  where
    helper :: [Int] -> String
    helper cs =
      case (maximum cs) of
        0 -> "==========\n0123456789\n"
        m -> concat [map (draw m) cs, "\n", helper $ map (sand m) cs]
               where
                 draw n c | n == c = '*'   | otherwise = ' '
                 sand n c | n == c = c - 1 | otherwise = c
    counts :: [Int] -> [Int]
    counts = foldr incrementCount $ replicate 10 0
      where incrementCount d cs = replaceAt d (cs !! d + 1) cs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = first ++ (x:rest)
  where (first, (_:rest)) = splitAt i xs
