module Week4.SieveOfSundaram where

sieveSundaram :: Integer -> [Integer]
sieveSundaram 0 = []
sieveSundaram n = produceSieveNums $ n `div` 2

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

pr1 :: Integer ->[(Integer, Integer)]
pr1 num = [(x,y) | x <- [1 .. num], y <- [1..num], let z = x + y + 2*x*y]

pr2 :: Integer -> [(Integer, Integer)]
pr2 p = map (\x -> case x of (z1, z2) -> (2*z1 +1, 2*z2+1)) $ pr1 p

produceSieveNums :: Integer -> [Integer]
produceSieveNums num = [z | x <- [1 .. num], y <- [1..num],  let z = x + y + 2*x*y]
