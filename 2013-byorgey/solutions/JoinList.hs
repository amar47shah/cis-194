{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Sized

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

testIndexJ :: Int -> JoinList Size Char -> Bool
testIndexJ i jl = indexJ i jl == (jlToList jl !!? i)

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0      = Nothing
indexJ _  Empty         = Nothing
indexJ i (Single _ x)
   | i > 0              = Nothing
   | otherwise          = Just x
indexJ i (Append t l r)
   | overC >= 0         = Nothing
   | overL <  0         = indexJ i     l
   | otherwise          = indexJ overL r
  where overC = i - (getSize . size) t
        overL = i - (getSize . size . tag) l
