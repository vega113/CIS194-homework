module LocalMaxima where

localMaxima :: [Integer] -> [Integer]
localMaxima inList = snd (localMaximaAcc(inList, []))

localMaximaAcc :: ([Integer], [Integer]) -> ([Integer], [Integer])
localMaximaAcc x =
  case x of
    (x1 : x2 : x3 : xs, acc) -> let
      nextAcc = if x2 > x1 && x2 > x3 then acc ++ [x2] else acc
      in localMaximaAcc(x2 : x3 : xs, nextAcc)
    _ -> x