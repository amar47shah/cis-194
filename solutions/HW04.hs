{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.Function (on)
import Data.List (dropWhileEnd, intersperse)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

example :: Num a => Poly a
example = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) = (==) `on` fromPoly

fromPoly :: (Num a, Eq a) => Poly a -> [a]
fromPoly (P (x:xs)) = x : dropWhileEnd (== 0) xs
fromPoly (P _     ) = 0 : []

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show = concat . intersperse " + " . reverse . toTerms

toTerms :: (Num a, Eq a, Show a) => Poly a -> [String]
toTerms = removeZeroes . filter (not . null) .
          (map (uncurry toTerm) .) zipWithIndex . fromPoly

removeZeroes :: [String] -> [String]
removeZeroes xs@(_:_:_) = dropWhile (== "0") xs
removeZeroes xs = xs

zipWithIndex :: [b] -> [(Integer, b)]
zipWithIndex = zip [0..]

toTerm :: (Num a, Eq a, Show a, Num b, Eq b, Show b) => a -> b -> String
toTerm 0   c  = show c
toTerm _   0  =           ""
toTerm 1 (-1) =           "-x"
toTerm 1   1  =            "x"
toTerm 1   c  = show c ++  "x"
toTerm d (-1) =           "-x^" ++ show d
toTerm d   1  =            "x^" ++ show d
toTerm d   c  = show c ++  "x^" ++ show d

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

