{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module HW05 where

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import GHC.Word (Word8)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

decodedBytes :: ByteString -> ByteString -> [Word8]
decodedBytes = BS.zipWith xor

-- Exercise 1 -----------------------------------------

extractSecret :: ByteString -> ByteString -> ByteString
extractSecret = ((BS.pack . filter (/= 0)) .) . decodedBytes

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret = liftA2 extractSecret `on` BS.readFile

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k out =
  BS.readFile (out ++ ".enc") >>= return . decrypt k >>= BS.writeFile out

decrypt :: ByteString -> ByteString -> ByteString
decrypt k e = BS.pack . decodedBytes e $ BS.cycle k

-- Apply what we've accomplished so far.
writeVictims :: IO ()
writeVictims = getSecret origPath modPath >>= (`decryptWithKey` outPath)
  where origPath = "../resources/clues/dog-original.jpg"
        modPath  = "../resources/clues/dog.jpg"
        outPath  = "../resources/clues/victims.json"

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = (decode <$>) . BS.readFile

transactions :: IO (Maybe [Transaction])
transactions = parseFile "../resources/clues/transactions.json"

victims :: IO (Maybe [TId])
victims = parseFile "../resources/clues/victims.json"

-- Exercise 4 -----------------------------------------

(<$$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$$>) = (<$>) <$> (<$>)

infixl 4 <$$$>

(<***>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<***>) = liftA2 (<*>)

infixl 4 <***>

findTs :: [TId] -> [Transaction] -> [Transaction]
findTs ids = filter $ (`elem` ids) . tid

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs badIdsPath transactionsPath =
  let ids = parseFile badIdsPath       :: IO (Maybe [TId])
      ts  = parseFile transactionsPath :: IO (Maybe [Transaction])
   in findTs <$$$> ids <***> ts

badTs :: IO (Maybe [Transaction])
badTs = getBadTs "../resources/clues/victims.json"
                 "../resources/clues/transactions.json"

testBadTsAndVictimsHaveEqualSize :: IO (Maybe Bool)
testBadTsAndVictimsHaveEqualSize =
  ((uncurry (==) . (length *** length)) .) . (,) <$$$> badTs <***> victims

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr recordT Map.empty
      where
  recordT (Transaction f t amt _) =
    Map.insertWith (+) t amt . Map.insertWith (+) f (negate amt)

damage :: IO (Map String Integer)
damage = getFlow <$> fromJust <$> badTs

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldrWithKey sniff ("Unknown", 0)
  where sniff member flow (thief, haul)
         | flow > haul = (member, flow)
         | otherwise   = (thief, haul)

criminal :: IO String
criminal = getCriminal <$> damage

-- Exercise 7 -----------------------------------------

payers :: Map String Integer -> [String]
payers m = (sortBy descendingGain) . Map.keys . Map.filter (> 0) $ m
  where descendingGain p q = negate (m Map.! p) `compare` negate (m Map.! q)

payees :: Map String Integer -> [String]
payees m = (sortBy ascendingLoss) . Map.keys . Map.filter (< 0) $ m
  where ascendingLoss p q = (m Map.! p) `compare` (m Map.! q)

repay :: Map String Integer -> String -> String -> TId -> (Transaction, Map String Integer)
repay m pr pe tid =
  (Transaction { from = pr, to = pe, amount = toPay, tid = tid }, m')
       where
  m'     = Map.adjust (subtract toPay) pr . Map.adjust (+ toPay) pe $ m
  toPay  = min canPay isOwed
  canPay =          m Map.! pr
  isOwed = negate $ m Map.! pe

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids
   | any null [payers m, payees m, tids] = []
   | otherwise = t : undoTs m' is
  where
     (t, m')    = repay m payer payee i
     payer : _  = payers m
     payee : _  = payees m
     i     : is = tids

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON outPath = BS.writeFile outPath . encode

-- Exercise 9 -----------------------------------------

-- Purposefully unsafe.
maybeFail :: String -> Maybe a -> Maybe a
maybeFail s Nothing = error s
maybeFail _ mx      = mx

doEverything ::
  FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath
  -> IO String
doEverything dog1 dog2 trans vict fids out =
  let flow = getFlow <$$$> maybeFail "No Transactions" <$> getBadTs vict trans
      ids  =               maybeFail "No ids"          <$> parseFile fids
   in getSecret dog1 dog2 >>= (`decryptWithKey` vict) >>
      undoTs <$$$> flow <***> ids >>= writeJSON out >>
      getCriminal . fromJust <$> flow

route :: [String] -> IO String
route (d1:d2:t:v:i:o:_) = doEverything d1 d2 t v i o
route _                 = doEverything
  "../resources/clues/dog-original.jpg"
  "../resources/clues/dog.jpg"
  "../resources/clues/transactions.json"
  "../resources/clues/victims.json"
  "../resources/clues/new-ids.json"
  "../resources/clues/new-transactions.json"

main :: IO ()
main = getArgs >>= route >>= putStrLn
