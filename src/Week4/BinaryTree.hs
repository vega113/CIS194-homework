module Week4.BinaryTree where

data Tree a
  = Leaf
  | Node Integer
         (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)

foldTreeStr :: [String] -> Tree String
foldTreeStr l = foldTree l

foldTree :: [a] -> Tree a
foldTree l = foldr (\a b -> insertB a b) Leaf l

compareNodes :: Tree a -> Tree a -> Bool
compareNodes Leaf Leaf =  True
compareNodes (Node levelL leftTreeL valueL rightTreeL) Leaf =  True
compareNodes Leaf (Node levelR leftTreeR valueR rightTreeR) =  False
compareNodes (Node levelL leftTreeL valueL rightTreeL) (Node levelR leftTreeR valueR rightTreeR) = levelL >= levelR

insertB :: a -> Tree a -> Tree a
insertB value Leaf = Node 0 Leaf value Leaf
insertB value (Node level mtl v mtr) =
  if compareNodes mtl mtr
    then Node (level + 1) (insertB value mtl) v mtr
    else Node (level + 1) mtl v (insertB value mtr)



printTree :: Tree String -> [String]
printTree Leaf = []
printTree (Node level left value right) = (printTree left) ++ [(show (level, value))] ++ (printTree right)

extractTreeLevel :: Tree a -> Integer
extractTreeLevel Leaf = 0
extractTreeLevel (Node level _ _ _) = level

