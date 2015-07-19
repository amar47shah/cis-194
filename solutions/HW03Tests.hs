-- Test cases for HW 03

module HW03Tests where

import HW03
import Testing

-- Setup

state, state', state'' :: State
state   = extend empty  "a" 1
state'  = extend state  "b" 2 --(\v -> case v of "a" -> 1; "b" -> 2; _ -> 0)
state'' = extend state' "a" 5

-- Exercise 1 -----------------------------------------

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

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF2 "evalE test" evalE
             [ (empty, Var "a", 0)
             , (empty, Val 7, 7)
             , (empty, Op (Val 6) Plus (Val 2), 8)
             , (empty, Op (Val 6) Minus (Val 2), 4)
             , (empty, Op (Val 6) Times (Val 2), 12)
             , (empty, Op (Val 6) Divide (Val 2), 3)
             , (empty, Op (Val 6) Gt (Val 2), 1)
             , (empty, Op (Val 6) Ge (Val 2), 1)
             , (empty, Op (Val 2) Gt (Val 2), 0)
             , (empty, Op (Val 2) Ge (Val 2), 1)
             , (empty, Op (Val 6) Lt (Val 2), 0)
             , (empty, Op (Val 6) Le (Val 2), 0)
             , (empty, Op (Val 2) Lt (Val 2), 0)
             , (empty, Op (Val 2) Le (Val 2), 1)
             , (empty, Op (Val 2) Eql (Val 3), 0)
             , (empty, Op (Val 2) Eql (Val 2), 1)
             , (empty, Op (Val 2) Eql (Val 1), 0)
             , (state, Var "a", 1)
             , (state'', Var "a", 5)
             , (state, Op (Var "a") Eql (Val 1), 1)
             , (state', Op (Var "a") Plus (Var "b"), 3)
             , (state, Op (Val 3) Plus (Op (Val 1) Plus (Var "a")), 5)
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  ]
