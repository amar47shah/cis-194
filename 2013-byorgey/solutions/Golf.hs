module Golf (skips) where

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
-- Map (take 3) over tails to get sliding windows, and take length l - 2
-- to ensure all windows have length 3.
-- Filter to get windows with local maxima, and map to isolate maxima.
-- 90 chars:
localMaxima l = map (\[_,y,_] -> y) .
                  filter (\[x,y,z] -> x < y && y > z) .
                    take (length l - 2) .
                      map (take 3) $
                        tails l
