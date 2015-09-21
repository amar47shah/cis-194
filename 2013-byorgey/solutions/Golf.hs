module Golf (skips) where

-- Use scanr to get successively reduced values from right,
-- where right-most value is [] and subsequent values are cons-ed.
-- Use filter (not . null) to remove [] from list, although init
-- can safely do the same. 33 chars.
skips :: [a] -> [[a]]
skips = filter (not . null) . scanr (:) []
