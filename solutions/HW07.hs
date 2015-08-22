{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = return . f =<< mx

--alternative definitions
--of course, liftM == fmap == <$>
liftM', liftM'', liftM''' :: Monad m => (a -> b) -> m a -> m b
liftM' f mx = mx >>= return . f

liftM'' = (=<<) . (return .) --pointfree, derived from first liftM

liftM''' f mx = do
  x <- mx
  return $ f x

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = liftM2 sub (v !? i) (v !? j)
  where sub x y = v // [(i, y), (j, x)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = (sequence .) . map

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = (v !?) `mapM` is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v !?) <$> getRandomR (0, pred $ V.length v)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.fromList <$> replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n = (V.fromList <$>) . replicateM n . getRandomR

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = foldM randomSwap v $ reverse [1..pred $ V.length v]
  where randomSwap w i = getRandomR (0, i) >>= swap w i
        swap w i j = return $ w // [(i, w ! j), (j, w ! i)]

-- only swaps *from* indexes lower than i
randomSwap' :: Vector a -> Int -> Rnd (Vector a)
randomSwap' v i
 | i < 0 || i >= V.length v = return v
 | otherwise              = do
       j <- getRandomR (0, i)
       return $ v // [(i, v ! j), (j, v ! i)]

randomSwapFirst :: Vector a -> Rnd (Vector a)
randomSwapFirst v
 | V.length v < 2 = return v
 | otherwise      = do
       j <- getRandomR (1, pred $ V.length v)
       return $ v // [(0, v ! j), (j, v ! 0)]

-- Exercise 6 -----------------------------------------

-- Unsafe, i must be in range [0..pred $ V.length v]
partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (V.filter (< x) remaining, x, V.filter (>= x) remaining)
  where x = v ! i
        remaining = removeAt v i

removeAt :: Vector a -> Int -> Vector a
removeAt v i = V.take i v V.++ (V.drop . succ) i v

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
 | V.null v  = v
 | otherwise = qsort [ y | y <- xs, y < x ]
               <> (x `cons` qsort [y | y <- xs, y >= x])
   where (x, xs) = (V.head v, V.tail v)

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR = undefined

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
