module ValidateCard where

toDigits :: Integer -> [Integer]
toDigits x = snd (toDigitsAcc (x, []))


toDigitsAcc :: (Integer, [Integer]) -> (Integer, [Integer])
toDigitsAcc (n, xs)
  | n < 10 && n >= 0 = (n, n : xs)
  | n >= 10 = toDigitsAcc (n `div` 10, (n `mod` 10) : xs)
  | otherwise = (0, [0])

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverseList(toDigits x)


reverseList :: [Integer] -> [Integer]
reverseList l = snd (reverseListAcc (l, []))

reverseListAcc :: ([Integer], [Integer]) -> ([Integer], [Integer])
reverseListAcc (l, acc)
  | l == [] = (l, acc)
  | otherwise = reverseListAcc (tail l, head l : acc)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = snd (doubleEveryOtherAcc (reverseList l, []))

doubleEveryOtherAcc :: ([Integer], [Integer]) -> ([Integer], [Integer])
doubleEveryOtherAcc([], acc) = ([], acc)
doubleEveryOtherAcc ([x1], acc) = ([], x1 : acc)
doubleEveryOtherAcc ([x1, x2], acc) = ([], x2 * 2 : x1 : acc)
doubleEveryOtherAcc(x1:x2:xs, acc) = doubleEveryOtherAcc(xs, x2*2:x1:acc)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits(xs)


isValidCardNumber :: Integer -> Bool
isValidCardNumber cardNum = sumDigits(doubleEveryOther(toDigits(cardNum))) `mod` 10 == 0
