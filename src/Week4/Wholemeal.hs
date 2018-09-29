module Week4.Wholemeal
  ( fun1
  , fun1'
  , fun2
  , fun2'
  ) where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = x - 2
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' =
  (\l ->
     if null l
       then 1
       else head l - 2) .
  take 1 . filter even

calc :: Integer -> Integer
calc xx =
  case xx of
    1 -> 0
    xxx ->
      if even xxx
        then xxx `div` 2
        else 3 * xxx + 1

--Looks like the fun2 doesn't stop unless you start with with power of 2
fun2' :: Integer -> Integer
fun2' x = sum (takeWhile (> 1) (iterate calc x))
