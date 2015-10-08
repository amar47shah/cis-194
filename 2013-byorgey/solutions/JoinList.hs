{-# OPTIONS_GHC -Wall #-}
module JoinList where

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
