module HW01 (toDigits, toDigitsRev) where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

--not tail-recursive
toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
toDigitsRev _         = []

testToDigits :: Bool
testToDigits = map toDigits [1234, 12, 0, -17] ==
  [ [1, 2, 3, 4]
  , [1, 2]
  , []
  , []
  ]
