module Main where

import ValidateCard
import Hanoi
import LogAnalysis
import Log
import Golf
import LocalMaxima

--import Data.Set
import Data.Maybe
import Text.Printf
import System.Environment
--import System.Posix.Temp
import Control.Monad

--import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
--import Test.Tasty.HUnit

main :: IO ()
--main = print (hanoi 3 "a" "b" "c")
--main = do
--  handle <- openFile "src/error.log" ReadMode
--  contents <- hGetContents handle
--  let messages = parse contents
--  putStr (unlines (whatWentWrong messages))
--  hClose handle

main =
--  putStr (show (addOrAppendAtIndex [(1, []), (2, ["h", "e"])] "l" 2))
--  putStr (show (addOrAppendAllAtIndex [(1, []), (2, ["h", "e"])] "l" [1, 2]))
--  putStr (show (skips "hello!"))
--  putStr (show (divisors 12))
  putStr (show (localMaxima [2,3,4,1,5]))



