{-# OPTIONS_GHC -Wall #-}
module LogAnalysis (insert, parse, parseMessage) where

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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node l m'@(LogMessage _ t' _) r)
  | t < t'    = Node (insert m l) m'  r
  | otherwise = Node  l           m' (insert m r)
insert _ _ = error "MessageTree contains Unknown LogMessage!"
