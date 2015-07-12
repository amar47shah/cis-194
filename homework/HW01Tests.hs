-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, ds) = toRevDigits n == ds

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(123, [3, 2, 1]), (1234, [4, 3, 2, 1]), (10, [0, 1]), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (ds, doubleds) = doubleEveryOther ds == doubleds

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [ ([3, 2, 1], [3, 4, 1]),
               ([4, 3, 2, 1], [4, 6, 2, 2]),
               ([1, 0], [1, 0]),
               ([5], [5]),
               ([], [])
             ]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (ds, s) = sumDigits ds == s

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [ ([0], 0), ([5], 5), ([2, 3], 5), ([12, 34, 5], 15),
               ([10], 1), ([18], 9), ([12, 3], 6), ([], 0)
             ]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (number, valid) = luhn number == valid

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [ (5594589764218858, True), (1234567898765432, False),
               (4242424242424242, True), (4342424242424242, False)
             ]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, p, q, r, ms) = hanoi n p q r == ms

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [ (0,"a","b","c",[])
             , (1,"a","b","c",[("a","b")])
             , (2,"a","b","c",[("a","c"),("a","b"),("c","b")])
             , (3,"a","b","c",[("a","b"),("a","c"),("b","c")
                              ,("a","b")
                              ,("c","a"),("c","b"),("a","b")
                              ]
               )
             , (4,"a","b","c",[("a","c"),("a","b"),("c","b")
                              ,("a","c")
                              ,("b","a"),("b","c"),("a","c")
                              ,("a","b")
                              ,("c","b"),("c","a"),("b","a")
                              ,("c","b")
                              ,("a","c"),("a","b"),("c","b")
                              ]
               )
             , (-1,"a","b","c",[])
             , (1,"Oakland","Louisville","Baltimore",[("Oakland","Louisville")])
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
