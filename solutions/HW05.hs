{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<$>))
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, listToMaybe)
import GHC.Word (Word8)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

decodedBytes :: ByteString -> ByteString -> [Word8]
decodedBytes xs ys = uncurry xor <$> BS.zip xs ys

--pointfree definitions
decodedBytes', decodedBytes'' :: ByteString -> ByteString -> [Word8]
decodedBytes'  = (((uncurry xor) <$>) .) . BS.zip
decodedBytes'' = (fmap (uncurry xor) .) . BS.zip

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret origPath modPath = do
  original <- BS.readFile origPath
  modified <- BS.readFile modPath
  return . BS.pack . filter (/= 0) $ decodedBytes original modified

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key outPath = do
  let inPath = outPath ++ ".enc"
  encoded <- BS.readFile inPath
  decoded <- return . BS.pack . decodedBytes encoded $ BS.cycle key
  BS.writeFile outPath decoded

-- Apply what we've accomplished so far.
writeVictims :: IO ()
writeVictims = do
  let origPath   = "../resources/clues/dog-original.jpg"
  let modPath    = "../resources/clues/dog.jpg"
  let resultPath = "../resources/clues/victims.json"
  key <- getSecret origPath modPath
  decryptWithKey key resultPath

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = (decode <$>) . BS.readFile

-- in pointed notation
parseFile' :: FromJSON a => FilePath -> IO (Maybe a)
parseFile' jsonPath = decode <$> BS.readFile jsonPath

-- in do-notation
parseFile'' :: FromJSON a => FilePath -> IO (Maybe a)
parseFile'' jsonPath = do
  json <- BS.readFile jsonPath
  return $ decode json

transactions :: IO (Maybe [Transaction])
transactions = parseFile "../resources/clues/transactions.json"

victims :: IO (Maybe [TId])
victims = parseFile "../resources/clues/victims.json"

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs badIdsPath transactionsPath = do
  maybeIds <- parseFile badIdsPath       :: IO (Maybe [TId])
  maybeTs  <- parseFile transactionsPath :: IO (Maybe [Transaction])
  case (maybeIds, maybeTs) of
       (Just ids, Just ts) -> return . Just $ filter ((`elem` ids) . tid) ts
       _                   -> return Nothing

badTs :: IO (Maybe [Transaction])
badTs = getBadTs "../resources/clues/victims.json"
                 "../resources/clues/transactions.json"

testBadTsAndVictimsHaveEqualSize :: IO Bool
testBadTsAndVictimsHaveEqualSize = do
  Just ts <- badTs
  Just vs <- victims
  return $ length ts == length vs

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr recordT Map.empty
  where recordT (Transaction { from = f, to = t, amount = a }) m =
          amend t a $ amend f (negate a) m
        amend member flow m' =
          case Map.lookup member m' of
            Just current -> Map.insert member (flow + current) m'
            Nothing      -> Map.insert member  flow            m'

damage :: IO (Map String Integer)
damage = getFlow <$> fromJust <$> badTs

damage' :: IO (Map String Integer)
damage' = do Just ts <- badTs
             return $ getFlow ts

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

repay :: Map String Integer -> String -> String -> TId ->
         (Transaction, Map String Integer)
repay m payer payee t =
  let (canPay, isOwed) = (m Map.! payer, negate $ m Map.! payee)
   in let payback = min canPay isOwed
          m' = Map.adjust (subtract payback) payer $
               Map.adjust        (+ payback) payee m
       in (Transaction { from=payer, to=payee, amount=payback, tid=t }, m')

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids =
  case (maybePayer, maybePayee, maybeTid) of
    (Just payer, Just payee, Just tid) ->
      [transaction] ++ undoTs adjustedMap (tail tids)
      where (transaction, adjustedMap) = repay m payer payee tid
    _ -> []
  where maybePayer = listToMaybe $ payers m
        maybePayee = listToMaybe $ payees m
        maybeTid   = listToMaybe tids

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

