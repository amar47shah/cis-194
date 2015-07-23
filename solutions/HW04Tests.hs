module HW04Tests where

import HW04
import Testing

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF2 "equality test" (==)
             [ (P []         , P [0]        , True)
             , (P [0,0]      , P [0]        , True)
             , (P [1,2,3]    , P [1,2,3]    , True)
             , (P [1,2,3]    , P [1,2,4]    , False)
             , (P [0,1,2]    , P [1,2]      , False)
             , (P [0,1,2]    , P [0,1,2,0,0], True)
             , (P [0,1,0]    , P [0,1,0,0]  , True)
             , (P [0,1,0,0,0], P [0,1]      , True)
             ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF1 "show test" show
             [ (P []       , "0")
             , (P [0]      , "0")
             , (P [1]      , "1")
             , (P [-1]     , "-1")
             , (P [5]      , "5")
             , (P [0,0]    , "0")
             , (P [0,1]    , "x")
             , (P [0,-1]   , "-x")
             , (P [0,5]    , "5x")
             , (P [-3,5]   , "5x + -3")
             , (P [0,0,1]  , "x^2")
             , (P [1,0,1]  , "x^2 + 1")
             , (P [1,0,5]  , "5x^2 + 1")
             , (P [1,0,0,2], "2x^3 + 1")
             , (P [0,-1,2] , "2x^2 + -x")
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex2Tests
                  , ex3Tests
                  ]
