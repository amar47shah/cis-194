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

propIndexJ :: Int -> JoinList Size Char -> Bool
propIndexJ i jl = indexJ i jl == (jlToList jl !!? i)

propDropJ :: Int -> JoinList Size Char -> Bool
propDropJ i jl = jlToList (dropJ i jl) == drop i (jlToList jl)

test :: (Int -> JoinList Size Char -> Bool) -> Bool
test p = all (\i -> p i joinList) [-1..5]
  where joinList = Append 4 (Append 2 (Single 1 'D') (Single 1 'C'))
                            (Append 2 (Single 1 'A') (Single 1 'B'))

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _  Empty         = Nothing
indexJ i (Single _ x)
   | i > 0              = Nothing
   | i < 0              = Nothing
   | otherwise          = Just x
indexJ i (Append t l r)
   | overC >= 0         = Nothing
   | i     <  0         = Nothing
   | i     == 0         = indexJ 0     l
   | overL <  0         = indexJ i     l
   | otherwise          = indexJ overL r
  where overC = i - (getSize . size) t
        overL = i - (getSize . size . tag) l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _  Empty            = Empty
dropJ i jl@(Single _ _)
   | i > 0                = Empty
   | i < 0                = jl
   | otherwise            = jl
dropJ i jl@(Append t l r)
   | overC >= 0           = Empty
   | i     < 0            = jl
   | i     == 0           = jl
   | overL <  0           = Append (tag (dropJ i l) <> tag r) (dropJ i l) r
   | otherwise            = dropJ overL r
  where overC = i - (getSize . size) t
        overL = i - (getSize . size . tag) l
