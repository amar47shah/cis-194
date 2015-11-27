{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Arrow ((&&&), (***))
import Control.Monad (liftM2, replicateM)
import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                   deriving Show

attacking :: Battlefield -> Army
attacking = max 0 . min 3 . pred . attackers

defending :: Battlefield -> Army
defending = max 0 . min 2 . defenders

roll :: Army -> Rand StdGen [DieValue]
roll = fmap (reverse . sort) . flip replicateM die

match :: Battlefield -> Rand StdGen [(DieValue, DieValue)]
match = uncurry (liftM2 zip) . (roll *** roll) . (attacking &&& defending)

settle :: (DieValue, DieValue) -> Battlefield -> Battlefield
settle (a, d) b
 | a > d     = b { defenders = pred $ defenders b }
 | otherwise = b { attackers = pred $ attackers b }

battle :: Battlefield -> Rand StdGen Battlefield
battle b = foldr settle b <$> match b
