module HW04Tests where

import HW04
import Testing

-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF2 "equality test" (==)
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

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  ]
