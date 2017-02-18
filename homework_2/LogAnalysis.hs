{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char

-- Ex1
isStringNumber :: String -> Bool
isStringNumber ""     = False
isStringNumber (x:[]) = isNumber x
isStringNumber (x:s)  = (isNumber x) && isStringNumber s

parseTimeStampAndMessage :: String -> (TimeStamp, String)
parseTimeStampAndMessage s
  | length(w) < 2            = (-1, "")
  | isStringNumber (head(w)) = ((read (head(w)) :: Int),
                                (drop (length(head(w)) + 1) s))
  | otherwise                = (-1, "")
  where w = words s

parseErrLvlTimeStampAndMessage :: String -> (Int, (TimeStamp, String))
parseErrLvlTimeStampAndMessage s
  | length(w) < 3                          = (-1, (-1, ""))
  | isStringNumber fw && isStringNumber sw = (read fw :: Int,
                                              (read sw :: Int, remainder))
  | otherwise                              = (-1, (-1, ""))
  where 
    w = words s
    fw = head w
    sw = head (tail w)
    remainder = drop (length(fw) + length(sw) + 2) s

stripSuperfluousSpaces :: String -> String
stripSuperfluousSpaces ""          = ""
stripSuperfluousSpaces (' ':' ':s) = stripSuperfluousSpaces (' ':s)
stripSuperfluousSpaces (' ':s)     = ' ':stripSuperfluousSpaces s
stripSuperfluousSpaces (x:s)       = x:stripSuperfluousSpaces s

parseMessage :: String -> LogMessage
parseMessage p @ ('I':' ':s) 
  | t == (-1)  = Unknown p 
  | otherwise  = LogMessage Info t m
  where (t, m) = parseTimeStampAndMessage s
parseMessage p @ ('W':' ':s) 
  | t == (-1)  = Unknown p 
  | otherwise  = LogMessage Warning t m
  where (t, m) = parseTimeStampAndMessage s
parseMessage p @ ('E':' ':s) 
  | el == (-1) = Unknown p
  | otherwise  = LogMessage (Error el) t m
  where (el, (t, m)) = parseErrLvlTimeStampAndMessage s 
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Ex 2
getTimeStampFromLogMsg :: LogMessage -> TimeStamp
getTimeStampFromLogMsg (Unknown _) = (-1)
getTimeStampFromLogMsg (LogMessage Info t _) = t
getTimeStampFromLogMsg (LogMessage Warning t _) = t
getTimeStampFromLogMsg (LogMessage (Error _) t _) = t

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mtree = mtree
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node ltree nodelm rtree)
  | tstamp < nodetstamp = Node (insert lm ltree) nodelm rtree
  | otherwise = Node ltree nodelm (insert lm rtree)
  where
    tstamp = getTimeStampFromLogMsg lm
    nodetstamp = getTimeStampFromLogMsg nodelm
