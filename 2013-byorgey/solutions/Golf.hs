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
