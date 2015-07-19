-- Test cases for HW 03

module HW03Tests where

import HW03
import Testing

-- Exercise 1 -----------------------------------------

state, state', state'' :: State
state   = extend empty  "a" 1
state'  = extend state  "b" 2 --(\v -> case v of "a" -> 1; "b" -> 2; _ -> 0)
state'' = extend state' "a" 5

ex1Tests :: [Test]
ex1Tests = [ testF1 "empty test" empty
             [("a", 0), ("b", 0), ("this", 0), ("that", 0), ("anything", 0)]
           , testF4 "extend test" extend
             [ (empty, "a", 1, "a", 1)
             , (empty, "a", 1, "b", 0)
             , (state, "a", 2, "a", 2)
             , (state, "b", 3, "b", 3)
             , (state, "b", 3, "a", 1)
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = ex1Tests
