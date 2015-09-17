{-# OPTIONS_GHC -Wall #-}
module LogAnalysis (parse, parseMessage) where

import Log

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

parseMessageWords :: [String] -> LogMessage
parseMessageWords ("I"  :t:ws@(_:_)) = logMessageFrom  Info            t ws
parseMessageWords ("E":l:t:ws@(_:_)) = logMessageFrom (Error $ read l) t ws
parseMessageWords ("W"  :t:ws@(_:_)) = logMessageFrom  Warning         t ws
parseMessageWords ws                 = Unknown $ unwords ws

logMessageFrom :: MessageType -> String -> [String] -> LogMessage
logMessageFrom msgType time ws = LogMessage msgType (read time) $ unwords ws
