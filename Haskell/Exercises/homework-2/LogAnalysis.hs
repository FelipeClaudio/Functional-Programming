{-# OPTIONS_GHC-Wall #-}
{-# LANGUAGE BlockArguments #-}
module Exercises.LogAnalysis where
import Log

{-
  Examples:
    parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
    parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
    parseMessage "This is not in the right format" == Unknown "This is not in the right format"
-}

getElementAt :: String -> Int -> String
getElementAt stringList elementPosistion = last (take elementPosistion (words stringList))

parseMessage :: String-> LogMessage
parseMessage input
  | head (words input) == "E" = LogMessage (Error (read (getElementAt input 2):: Int)) (read (getElementAt input 3):: TimeStamp) (unwords (drop 3 (words input)))
  | head (words input) == "I" = LogMessage Info (read (getElementAt input 2):: TimeStamp) (unwords (drop 2 (words input)))
  | otherwise = Unknown input

parse :: String-> [LogMessage]
parse input
  | null (lines input) = []
  | otherwise = parseMessage (head (lines input)) : parse (unlines (tail (lines input)))