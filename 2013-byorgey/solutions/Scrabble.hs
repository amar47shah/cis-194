{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char (toUpper)
import Data.Monoid

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
   | c `elemAlpha` "QZ"         = Score 10
   | c `elemAlpha` "JX"         = Score 8
   | c `elemAlpha` "K"          = Score 5
   | c `elemAlpha` "FHVWY"      = Score 4
   | c `elemAlpha` "BCMP"       = Score 3
   | c `elemAlpha` "DG"         = Score 2
   | c `elemAlpha` "EAIONRTLSU" = Score 1
   | otherwise                  = Score 0
  where elemAlpha = elem . toUpper

scoreString :: String -> Score
scoreString = sum . map score
