{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches ps qs = length . filter (\(a, b) -> a == b) $ zip ps qs
          -- ps qs = length . filter (uncurry (==)) . zip ps $ qs
          -- ps = length . filter (uncurry (==)) . zip ps
          -- = (.) (length . filter (uncurry (==))) . zip
          -- = ((length . filter (uncurry (==))) .) . zip

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors ps = map (\c -> length . filter (== c) $ ps) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches ps qs = sum . map (uncurry min) $ zip (countColors ps) (countColors qs)
     -- ps = sum . map (uncurry min) . (zip . countColors $ ps) . countColors
     -- = (sum .) . ((map (uncurry min) .) . ((. countColors) . (zip . countColors)))
     -- = (sum .) . ((map (uncurry min) .) . (flip (.) (zip . countColors) (. countColors)))
     -- using Control.Arrow ((***)):
     -- = (sum .) . ((map (uncurry min) .) . (uncurry zip .) . curry (countColors *** countColors))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s c = Move c (exactMatches s c) (nonExactMatches s c)
    where nonExactMatches s' c' = matches s' c' - exactMatches s' c'

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move c _ _) s = m == getMove s c

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent
         -- m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n | n < 1 = []
allCodes 1         = map (:[]) colors
allCodes n         = longerByOne . allCodes . pred $ n
    where longerByOne = concatMap $ \code -> [ code ++ [peg] | peg <- colors ]

--with Data.Sequence.replicateM and Data.Foldable.toList
--allCodes 0 = []
--allCodes n = map toList . replicateM n $ colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve s = reverse . snd . solveHelper (getMove s) $ (allCodes . length $ s, [])

solveHelper :: (Code -> Move) -> ([Code], [Move]) -> ([Code], [Move])
solveHelper _ t@([], _) = t
solveHelper g (cs@(c:[]), ms@(m:_))
 | c == codeFromMove m = (cs, ms)
 | otherwise           = (cs, g c : ms)
solveHelper g (cs@(c:_), ms) = solveHelper g (filterCodes (g c) cs, g c : ms)

codeFromMove :: Move -> Code
codeFromMove (Move c _ _) = c

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
