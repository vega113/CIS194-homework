module Golf where
import Data.List

divisors :: Int -> [Int]
divisors n = filter ((==0) . rem n) [1 .. n `div` 2] ++ [n]

replaceAt :: (a -> Bool) -> a -> [a] -> [a]
replaceAt whereF replaceWith =
  foldl
    (\acc origElem ->
       if whereF origElem
         then acc ++ [replaceWith]
         else acc ++ [origElem])
    []

addOrAppendAtIndex :: [(Int, [a])] -> a -> Int -> [(Int, [a])]
addOrAppendAtIndex accList objAt indexAt =
  let whereF x =
        case x of
          (index, obj) -> index == indexAt
      atElem = find whereF accList
   in case atElem of
        Just (_, listToAppend) -> replaceAt whereF (indexAt, listToAppend ++ [objAt]) accList
        _ -> accList ++ [(indexAt, [objAt])]


addOrAppendAllAtIndex :: [(Int, [a])] -> a -> [Int] -> [(Int, [a])]
addOrAppendAllAtIndex accListAt objAt divisors =
  snd
    (foldl
       (\x divisor ->
          case x of
            (index, accList) -> (index + 1, addOrAppendAtIndex accList objAt divisor))
       (1, accListAt)
       divisors
    )

skips :: [a] -> [[a]]
skips listOfA =
  map
    snd
    (snd
       (foldl
          (\acc obj ->
             case acc of
               (index, accList) -> (index + 1, addOrAppendAllAtIndex accList obj (divisors index)))
          (1, [])
          listOfA))