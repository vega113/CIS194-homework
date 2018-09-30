module Week4.MoreFolds where

xor :: [Bool] -> Bool
xor l = foldr (\a b -> if not a then b else not b) False l

map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\a y -> (f a) : y) [] l