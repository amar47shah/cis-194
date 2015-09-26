{-# OPTIONS_GHC -Wall #-}
module HW04Tests where

import HW04
import Control.Arrow ((&&&))
import Data.List (tails)

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "fun1' == fun1" $ prop_sameFun1
  , QC.testProperty "fun2' == fun2" $ forAll (positiveUpTo 1000) prop_sameFun2
  , QC.testProperty "foldTree result has correct heights" $
                    forAll (positiveUpTo 275) prop_foldTreeCorrectHeights
  , QC.testProperty "foldTree result is balanced" $
                    forAll (positiveUpTo 275) prop_foldTreeBalanced
  , QC.testProperty "xor iff True count odd" $ prop_xorTrueCountOdd
  ]

prop_sameFun1 :: [Integer] -> Bool
prop_sameFun1 = same fun1 fun1'

prop_sameFun2 :: Integer -> Bool
prop_sameFun2 = same fun2 fun2'

prop_foldTreeCorrectHeights :: Integer -> Bool
prop_foldTreeCorrectHeights n = heightsCorrect $ foldTree [1..n]

prop_foldTreeBalanced :: Integer -> Bool
prop_foldTreeBalanced = same heightFor integerLog2

prop_xorTrueCountOdd :: [Bool] -> Bool
prop_xorTrueCountOdd = same xor (odd . length . filter id)

same :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
same f g = uncurry (==) . (f &&& g)

heightsCorrect :: Tree a -> Bool
heightsCorrect Leaf                 = True
heightsCorrect (Node h Leaf _ Leaf) = h == 0
heightsCorrect (Node h Leaf _ (Node h' _ _ _)) = h == h' + 1
heightsCorrect (Node h (Node h' _ _ _) _ Leaf) = h == h' + 1
heightsCorrect (Node h l@(Node hl _ _ _) _ r@(Node hr _ _ _)) =
  h == max hl hr + 1 && heightsCorrect l && heightsCorrect r

integerLog2 :: Integer -> Integer
integerLog2 = toInteger . length . takeWhile (> 1) . iterate (`div` 2)

heightFor :: Integer -> Integer
heightFor n = height $ foldTree [1..n]
  where height (Node h _ _ _) = h
        height  Leaf          = -1

positiveUpTo :: Integer -> Gen Integer
positiveUpTo = choose . ((,) 1)

-- Verify that foldTree's results have correct heights, up to 257 elts.
-- Should return True
testHeightsCorrect :: Bool
testHeightsCorrect = all (heightsCorrect . foldTree) $ tails ([1..257] :: [Integer])

-- Verify that height increases at 2^n elements.
testBalancedInsert :: Bool
testBalancedInsert = all (\(n, h) -> heightFor n == h) $
                     [ (  0, -1)
                     , (  1,  0)
                     , (  2,  1), (  3,  1)
                     , (  4,  2), (  7,  2)
                     , (  8,  3), ( 15,  3)
                     , ( 16,  4), ( 31,  4)
                     , ( 32,  5), ( 63,  5)
                     , ( 64,  6), (127,  6)
                     , (128,  7), (255,  7)
                     , (256,  8), (511,  8)
                     , (512,  9)
                     ]
