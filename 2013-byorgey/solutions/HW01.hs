--
-- See 2015-noamz/solutions/HW01.hs for solutions to similar problems.
--
module HW01 (toDigits, toDigitsRev) where

--not tail-recursive
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
toDigitsRev _         = []

--tail-recursive
toDigits' :: Integer -> [Integer]
toDigits' n = go n []
  where go n xs | n > 0 = go (n `div` 10) $ n `mod` 10 : xs
        go _ xs         = xs

test_digitize :: (Integer -> [Integer]) -> Bool
test_digitize f = map f [1234, 12, 0, -17] == [[1,2,3,4], [1,2], [], []]
