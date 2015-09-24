{-# OPTIONS_GHC -Wall #-}
module HW04Tests where

import HW04
import Control.Arrow ((&&&))

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
