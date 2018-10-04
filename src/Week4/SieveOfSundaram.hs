module Week4.SieveOfSundaram where

sieveSundaram :: Integer -> [Integer]
sieveSundaram 0 = []
sieveSundaram 1 = []
sieveSundaram n =
  let newN = (n `div` 2)
      sieveNums = produceSieveNums newN
   in 2 : filter (<= n) (map (\x -> 2 * x + 1) (filter (`notElem` sieveNums) [1 .. newN]))

produceSieveNums :: Integer -> [Integer]
produceSieveNums num = [z | x <- [1 .. num], y <- [1..x], let z = x + y + 2*x*y, z <= num]
