{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Buffer
import Editor
import Scrabble
import Sized

import Data.List (intercalate)
import Data.Monoid ((<>))

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- Exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
j +++ k = Append (tag j <> tag k) j k

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single t _)   = t
tag (Append t _ _) = t

--------------------------------------------------------------------------------

(!!?) :: [a] -> Int -> Maybe a
[] !!? _         = Nothing
_  !!? i | i < 0 = Nothing
(x:_)  !!? 0     = Just x
(_:xs) !!? i     = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

propIndexJ :: Int -> JoinList Size Char -> Bool
propIndexJ i jl = indexJ i jl == (jlToList jl !!? i)

propDropJ :: Int -> JoinList Size Char -> Bool
propDropJ i jl = jlToList (dropJ i jl) == drop i (jlToList jl)

propTakeJ :: Int -> JoinList Size Char -> Bool
propTakeJ i jl = jlToList (takeJ i jl) == take i (jlToList jl)

test :: (Int -> JoinList Size Char -> Bool) -> Bool
test p = all (\i -> p i joinList) [-1..5]
  where joinList = Append 4 (Append 2 (Single 1 'D') (Single 1 'C'))
                            (Append 2 (Single 1 'A') (Single 1 'B'))

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _  Empty         = Nothing
indexJ i (Single _ x)
   | i >  0             = Nothing
   | i == 0             = Just x
   | otherwise          = Nothing
indexJ i (Append t l r)
   | i >= wholeSize     = Nothing
   | i >  leftSize      = indexJ (i - leftSize) r
   | i == leftSize      = indexJ 0 r
   | i >  0             = indexJ i l
   | i == 0             = indexJ 0 l
   | otherwise          = Nothing
  where wholeSize = getSize . size $ t
        leftSize  = getSize . size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _  Empty            = Empty
dropJ i jl@(Single _ _)
   | i >  0               = Empty
   | i == 0               = jl
   | otherwise            = jl
dropJ i jl@(Append t l r)
   | i >= wholeSize       = Empty
   | i >  leftSize        = dropJ (i - leftSize) r
   | i == leftSize        = r
   | i >  0               = let l' = dropJ i l in Append (tag l' <> tag r) l' r
   | i == 0               = jl
   | otherwise            = jl
  where wholeSize = getSize . size $ t
        leftSize  = getSize . size $ tag l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty             = Empty
takeJ i jl@(Single _ _)
   | i >  0               = jl
   | i == 0               = Empty
   | otherwise            = Empty
takeJ i jl@(Append t l r)
   | i >= wholeSize       = jl
   | i >  leftSize        = let r' = takeJ (i - leftSize) r
                             in Append (tag l <> tag r') l r'
   | i == leftSize        = l
   | i >  0               = takeJ i l
   | i == 0               = Empty
   | otherwise            = Empty
  where wholeSize = getSize . size $ t
        leftSize  = getSize . size $ tag l

--------------------------------------------------------------------------------

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine = scoreString >>= Single

--------------------------------------------------------------------------------

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString = intercalate " " . jlToList
  fromString s = Single (scoreString s, 1) s
  line = indexJ
  replaceLine i s jl = takeJ i jl +++ fromString s +++ dropJ (i + 1) jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag


main :: IO ()
main = runEditor editor $
       ( fromString "This buffer is for notes you don't want to save, and for"
         +++ fromString "evaluation of steam valve coefficients."
         +++ fromString "To load a different file, type the character L followed"
         +++ fromString "by the name of the file."
         :: JoinList (Score, Size) String
       )
