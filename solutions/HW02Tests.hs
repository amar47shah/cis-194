-- CIS 194, Spring 2015
--
-- Test cases for HW 02

module HW02Tests where

import HW02
import Testing


-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF2 "exactMatches test" exactMatches
             [ ([], [], 0)
             , ([Red], [Blue], 0)
             , ([Red, Green], [Blue, Green], 1)
             , ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
             ]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "countColors test" countColors
             [ ([Red, Blue, Yellow, Purple], [1, 0, 1, 1, 0, 1])
             , ([Green, Blue, Green, Orange], [0, 2, 1, 0, 1, 0])
             ]
           , testF2 "matches test" matches
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3)
             , ([], [], 0)
             , ([Red, Blue, Yellow, Orange], [], 0)
             , ([Red], [Green], 0)
             , ([Red], [Red], 1)
             , ([Red, Blue], [Green, Green], 0)
             , ([Red, Blue], [Blue, Green], 1)
             , ([Red, Red], [Red, Blue], 1)
             , ([Red, Red], [Red, Red], 2)
             , ([Red, Red, Red], [Red, Red], 2)
             , ([Red, Green, Blue], [Green, Blue, Red], 3)
             ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF2 "getMove test" getMove
             [ ( [], [], Move [] 0 0 )
             , ( [Red], [Blue], Move [Blue] 0 0 )
             , ( [Red, Blue], [Blue, Red], Move [Blue, Red] 0 2 )
             , ( [Red, Blue], [Blue], Move [Blue] 0 1 )
             , ( [Red, Blue], [Red], Move [Red] 1 0 )
             , ( [Red, Blue, Red, Blue]
               , [Blue, Red, Blue, Red]
               , Move [Blue, Red, Blue, Red] 0 4
               )
             , ( [Red, Blue, Yellow, Orange]
               , [Red, Orange, Orange, Blue]
               , Move [Red, Orange, Orange, Blue] 1 2
               )
             , ( [Blue, Green, Orange, Red]
               , [Blue, Red, Green, Orange]
               , Move [Blue, Red, Green, Orange] 1 3
               )
             ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "isConsistent test" isConsistent
             [ (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Yellow, Purple],
               True)
             , (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Red, Purple],
               False)
             ]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF2 "filterCodes test" filterCodes
             [ ( Move [Red, Red, Blue, Green] 1 1
               , [[Red, Blue, Red, Purple], [Red, Blue, Yellow, Purple]]
               , [[Red, Blue, Yellow, Purple]])
             ]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = []

-- Bonus ----------------------------------------------

bonusTests :: [Test]
bonusTests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , bonusTests
                  ]
