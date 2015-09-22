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

histogram :: [Int] -> String
histogram ns = h $ counts ns
  where
    h cs =
      case (maximum cs) of
        0 -> "==========\n0123456789\n"
        m -> map (\c -> if c == m then '*' else ' ') cs ++ "\n" ++
             (h $ map (\c -> if c == m then c - 1 else c) cs)

counts :: [Int] -> [Int]
counts = foldr (\d cs -> replaceAt d (cs !! d + 1) cs) (replicate 10 0)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n y xs = a ++ (y:b)
  where (a, (_:b)) = splitAt n xs
