{-# OPTIONS_GHC-Wall #-}
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

insert :: LogMessage-> MessageTree-> MessageTree
insert (LogMessage messageType timeStamp text) (Node leftNode (LogMessage messageType2 timeStamp2 text2) rightNode)
  | timeStamp >= timeStamp2 = Node leftNode (LogMessage messageType2 timeStamp2 text2) (insert (LogMessage messageType timeStamp text) rightNode)
  | timeStamp < timeStamp2 = Node (insert (LogMessage messageType timeStamp text) leftNode) (LogMessage messageType2 timeStamp2 text2) rightNode
insert (LogMessage messageType timeStamp text) Leaf = Node Leaf (LogMessage messageType timeStamp text) Leaf
insert (Unknown _) messageTree = messageTree
insert (LogMessage {}) (Node _ (Unknown _) _) = Leaf --Impossible case add for removing compilation warnings
insert (LogMessage {}) (Node _ (LogMessage {}) _) = Leaf  --Impossible case add for removing compilation warnings

build :: [LogMessage]-> MessageTree
build [] = Leaf
build [logMessage] = insert logMessage Leaf
build (logMessage:logMessageArray) = insert logMessage (build logMessageArray)

inOrder :: MessageTree-> [LogMessage]
inOrder (Node leftNode (LogMessage messageType2 timeStamp2 text2) rightNode) = inOrder leftNode ++ [LogMessage messageType2 timeStamp2 text2] ++ inOrder rightNode
inOrder Leaf = []
inOrder (Node _ (Unknown _) _) = []

removeFirst :: [a] -> [a]
removeFirst [] = []
removeFirst (_:xs) = xs

removeNonErrorLogMessages :: LogMessage -> Bool
removeNonErrorLogMessages (LogMessage (Error level) _ _) = level >= 50
removeNonErrorLogMessages _ = False

whatWentWrong :: [LogMessage]-> [String]
whatWentWrong [] = []
whatWentWrong [LogMessage (Error level) _ text]
  | level >= 50 = [text]
  | otherwise = []
whatWentWrong [Unknown _] = []
whatWentWrong [LogMessage {}] = []
whatWentWrong logMessages =
  let filteredOrderedList = inOrder (build (filter removeNonErrorLogMessages logMessages))
  in whatWentWrong [head filteredOrderedList] ++ whatWentWrong (removeFirst filteredOrderedList)