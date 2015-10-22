{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee

import Data.Monoid ((<>))

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
