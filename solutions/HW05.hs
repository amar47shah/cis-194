{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<$>))
import Data.Map.Strict (Map)
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
getBadTs = undefined

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

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

