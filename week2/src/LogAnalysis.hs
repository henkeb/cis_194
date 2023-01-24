{-# OPTIONS_GHC -Wall #-}

module LogAnalysis (someFunc) where

import           Log

parseMessage :: [String] -> LogMessage
parseMessage ("E" : c : t : m) = LogMessage (Error (read c)) (read t) (unwords m)
parseMessage ("W" : t : m) = LogMessage Warning (read t) (unwords m)
parseMessage ("I" : t : m) = LogMessage Info (read t) (unwords m)
parseMessage p = Unknown (unwords p)

someFunc :: IO ()
someFunc = print $ parseMessage $ words "E 2 562 help help"
