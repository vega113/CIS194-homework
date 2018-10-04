module LogAnalysis where

import Data.List
import Log

parseMessage :: String -> LogMessage
parseMessage line =
  let (maybeMessageType, words1) = parseLevelAcc $ words line
      (maybeTimestamp, words2) = parseTimestampAcc words1
      maybeMessage = maybeMessagePartsToMessage maybeMessageType maybeTimestamp words2
   in case maybeMessage of
        Just message -> message
        _ -> Unknown line

parse :: String -> [LogMessage]
parse str = fmap parseMessage (lines str)

parseLevelAcc :: [String] -> (Maybe MessageType, [String])
parseLevelAcc [] = (Nothing, [])
parseLevelAcc x =
  case x of
    x1:x2:xs ->
      case x1 of
        "I" -> (Just Info, x2 : xs)
        "W" -> (Just Info, x2 : xs)
        "E" -> (Just (Error (read x2)), xs)
        _ -> (Nothing, x)

parseTimestampAcc :: [String] -> (Maybe Int, [String])
parseTimestampAcc [] = (Nothing, [])
parseTimestampAcc x =
  case x of
    timestampStr:xs -> (Just $ read timestampStr, xs)

maybeMessagePartsToMessage :: Maybe MessageType -> Maybe Int -> [String] -> Maybe LogMessage
maybeMessagePartsToMessage (Just messageType) (Just timestamp) msg =
  Just $ LogMessage messageType timestamp (unwords msg)
maybeMessagePartsToMessage _ _ _ = Nothing

compareLogMessage :: LogMessage -> LogMessage -> Bool
compareLogMessage (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 < t2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert l Leaf = Node Leaf l Leaf
insert l (Node mtl logMessage mtr) =
  if compareLogMessage l logMessage
    then Node (LogAnalysis.insert l mtl) logMessage mtr
    else Node mtl logMessage (LogAnalysis.insert l mtr)

build :: [LogMessage] -> MessageTree
build = foldl (flip LogAnalysis.insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = inOrder left ++ [log] ++ inOrder right

extractSeverity :: LogMessage -> Int
extractSeverity (LogMessage (Error s) _ _) = s
extractSeverity _ = 0

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = printTree (build (filter (\m -> extractSeverity m > 50) messages))

printTree :: MessageTree -> [String]
printTree Leaf = []
printTree (Node left log right) = printTree left ++ [show log] ++ printTree right