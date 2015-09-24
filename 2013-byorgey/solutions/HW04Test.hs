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
  ]

prop_sameFun1 :: [Integer] -> Bool
prop_sameFun1 = uncurry (==) . (fun1 &&& fun1')
