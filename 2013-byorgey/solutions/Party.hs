{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee

import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Tree

--------------------------------------------------------------------------------

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
e `glCons` GL es f = GL (e:es) $ f + empFun e

instance Monoid Fun where
  mempty = 0
  mappend = (+)

instance Monoid GuestList where
  mempty = GL mempty mempty
  GL les lf `mappend` GL res rf = GL (les <> res) (lf <> rf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--------------------------------------------------------------------------------

-- Exercise 2

-- Bottom-up folding of each tree in forest, left-to-right
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node rl ts) = f rl $ map (treeFold f) ts

-- Top-down folding of each tree in forest, left-to-right
treeFold' :: (a -> b) -> (b -> b -> b) -> Tree a -> b
treeFold' f bop (Node rl ts) = foldl' accumulate initial ts
  where accumulate folded t = folded `bop` treeFold' f bop t
        initial = f rl

--------------------------------------------------------------------------------

-- Exercise 3

-- We get a boss and subdivision guestlists with and without the managers
-- If we invite the boss, don't invite the subdivision managers, and vice versa
-- Return one guestlist with the boss and one without

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss = glCons boss . mconcat . map snd &&& mconcat . map fst

--------------------------------------------------------------------------------

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel
