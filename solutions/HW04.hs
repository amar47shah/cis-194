{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.Function (on)
import Data.List (dropWhileEnd, foldl', intersperse)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) = (==) `on` fromPoly

fromPoly :: (Num a, Eq a) => Poly a -> [a]
fromPoly (P (c:cs)) = c : dropWhileEnd (== 0) cs
fromPoly (P _     ) = 0 : []

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show = concat . intersperse " + " . reverse . toTerms

toTerms :: (Num a, Eq a, Show a) => Poly a -> [String]
toTerms = dropZeroTerm . filter (not . null) .
          (map (uncurry toTerm) .) zipWithIndex . fromPoly

dropZeroTerm:: [String] -> [String]
dropZeroTerm ("0":ts@(_:_)) = ts
dropZeroTerm ts             = ts

zipWithIndex :: [b] -> [(Int, b)]
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
plus (P cs) (P ds) = P . uncurry (zipWith (+)) $ paddedSummands cs ds

padr :: a -> Int -> [a] -> [a]
padr c n cs = cs ++ replicate n c

padLength :: [a] -> [a] -> Int
padLength cs ds = (max `on` length) cs ds - (min `on` length) cs ds

paddedSummands :: (Num a) => [a] -> [a] -> ([a], [a])
paddedSummands cs ds
     | length cs > length ds = (cs, pad ds)
     | otherwise             = (pad cs, ds)
  where pad = padr 0 $ padLength cs ds


-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times p (P cs) = sum . map (uncurry $ timesTerm p) $ zipWithIndex cs
    where timesTerm (P cs') d = timesScalar . P $ padl 0 d cs'

padl :: a -> Int -> [a] -> [a]
padl c n cs = replicate n c ++ cs

timesScalar :: Num a => Poly a -> a -> Poly a
timesScalar (P cs) s = P . map (* s) $ cs

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P cs) = P . map negate $ cs
    fromInteger = P . (: []) . fromInteger
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P cs) arg = fst $ foldl' (acc arg) (0, 0 :: Int) cs
    where acc arg' (s, d) c = (s + c * arg'^d, d + 1)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

