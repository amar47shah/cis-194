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
  , QC.testProperty "fun2' == fun2" $ forAll smallPos prop_sameFun2
  ]

prop_sameFun1 :: [Integer] -> Bool
prop_sameFun1 = same fun1 fun1'

prop_sameFun2 :: Integer -> Bool
prop_sameFun2 = same fun2 fun2'

same :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
same f g = uncurry (==) . (f &&& g)

smallPos :: Gen Integer
smallPos = choose (1, 1000)

-- TODO: Make these into unit tests!

-- Verify that foldTree's results have correct heights, up to 257 elts.
-- Should return True
testHeightsCorrect :: Bool
testHeightsCorrect = all (heightsCorrect . foldTree) $ tails ([1..257] :: [Integer])
  where
    heightsCorrect :: Tree a -> Bool
    heightsCorrect Leaf                 = True
    heightsCorrect (Node h Leaf _ Leaf) = h == 0
    heightsCorrect (Node h Leaf _ (Node h' _ _ _)) = h == h' + 1
    heightsCorrect (Node h (Node h' _ _ _) _ Leaf) = h == h' + 1
    heightsCorrect (Node h l@(Node hl _ _ _) _ r@(Node hr _ _ _)) =
      h == max hl hr + 1 && heightsCorrect l && heightsCorrect r

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
  where
    height (Node h _ _ _) = h
    height  Leaf          = -1
    heightFor :: Integer -> Integer
    heightFor n = height $ foldTree [1..n]
