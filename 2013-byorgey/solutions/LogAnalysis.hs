{-# OPTIONS_GHC -Wall #-}
module LogAnalysis (parse, parseMessage) where

import Log

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

parseMessageWords :: [String] -> LogMessage
parseMessageWords ("I"  :t:m@(_:_)) = LogMessage  Info            (read t) $ unwords m
parseMessageWords ("E":l:t:m@(_:_)) = LogMessage (Error $ read l) (read t) $ unwords m
parseMessageWords ("W"  :t:m@(_:_)) = LogMessage  Warning         (read t) $ unwords m
parseMessageWords ws                = Unknown $ unwords ws
