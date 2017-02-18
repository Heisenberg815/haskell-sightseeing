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
