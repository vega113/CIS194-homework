module Histogram where

import Data.List
import Data.Ord
import Data.Char

digits = reverse [0..9]
header = "\n" ++ map (const '=') digits ++ "\n" ++ map intToDigit (reverse digits)

frequencies :: [Int] -> [(Int, Int)]
frequencies inList =
  let groupAcc = group $ sort inList
   in foldl
        (\b a ->
           case a of
             (x:xs) -> b ++ [(x, length a)])
        []
        groupAcc

frequenciesToString :: [(Int, Int)] -> String
frequenciesToString m =
  snd
    (foldl
       (\b a ->
          case b of
            (isFirst, acc) ->
              let freqsHigherThanCurrent = filter (\x -> snd x > a) m
               in if not (null freqsHigherThanCurrent)
                    then let (keys, frequencies) = unzip freqsHigherThanCurrent
                             maxInLine = maximum keys
                             currentLine =
                               foldl
                                 (\b1 a1 ->
                                    if a1 `elem` keys
                                      then b1 ++ "*"
                                      else b1 ++ " ")
                                 []
                                 [0 .. maxInLine]
                          in (if isFirst
                                then (False, currentLine)
                                else (False, acc ++ "\n" ++ currentLine))
                    else b)
       (True, [])
       digits)

histogram :: [Int] -> String
histogram l = (frequenciesToString $ frequencies l) ++ header