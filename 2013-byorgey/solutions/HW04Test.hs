{-# OPTIONS_GHC -Wall #-}
module HW04Tests where

import HW04

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "" $ prop_sameFun1
  ]

prop_sameFun1 :: [Integer] -> Bool
prop_sameFun1 xs = fun1 xs == fun1' xs
