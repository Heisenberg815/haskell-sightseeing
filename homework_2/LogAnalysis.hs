{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char

-- Note on the Ex1: the parse function would not work properly if there was
-- multiple spaces between words in the log sample. It could be solved by
-- stripping the unecessary spaces from the input logs.

-- Ex 1
isStringNumber :: String -> Bool
isStringNumber ""     = False
isStringNumber (x:[]) = isNumber x
isStringNumber (x:s)  = (isNumber x) && isStringNumber s

-- |Parse a timestamp and a content message in a String such as "42 Hello!"
-- In that case it would return (42, "Hello!"). If the input String cannot be
-- parsed in such a way, by convention we return (-1, "").
parseTimeStampAndMessage :: String -> (TimeStamp, String)
parseTimeStampAndMessage s
  | length(w) < 2            = (-1, "")
  | isStringNumber (head(w)) = ((read (head(w)) :: Int),
                                (drop (length(head(w)) + 1) s))
  | otherwise                = (-1, "")
  where w = words s

-- |Parse a error level, a timestamp and a content messsage in a String such as
-- "77 42 Hello!" In that case it would return (77, (42, "Hello!")).
-- If the input String cannot be parsed in such a way, by convention we
-- return (-1, (-1, "")).
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
insert lm Leaf           = Node Leaf lm Leaf
insert lm (Node ltree nodelm rtree)
  | tstamp < nodetstamp  = Node (insert lm ltree) nodelm rtree
  | otherwise            = Node ltree nodelm (insert lm rtree)
  where
    tstamp     = getTimeStampFromLogMsg lm
    nodetstamp = getTimeStampFromLogMsg nodelm

-- Ex 3
build :: [LogMessage] -> MessageTree
build []    = Leaf
build (x:s) = insert x (build s)

-- Ex 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node ltree nodelm rtree) = (inOrder ltree) ++ [nodelm]
                                    ++ (inOrder rtree)

-- Ex 5
getStringFromLogMsg :: LogMessage -> String
getStringFromLogMsg (Unknown s) = s
getStringFromLogMsg (LogMessage Info _ s) = s
getStringFromLogMsg (LogMessage Warning _ s) = s
getStringFromLogMsg (LogMessage (Error _) _ s) = s

isRelevant :: LogMessage -> Bool
isRelevant (Unknown _)                 = False
isRelevant (LogMessage Info _ _)       = False
isRelevant (LogMessage Warning _ _)    = False
isRelevant (LogMessage (Error el) _ _) = el >= 50

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = map getStringFromLogMsg (filter isRelevant sorted)
  where sorted  = inOrder (build l)
