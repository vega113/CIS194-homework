module Week4.BinaryTree where

data Tree a
  = Leaf
  | Node Integer
         (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertB Leaf

compareNodes :: Tree a -> Tree a -> Bool
compareNodes Leaf Leaf = False
compareNodes (Node _ _ _ _) Leaf = False
compareNodes Leaf (Node _ _ _ _) = True
compareNodes (Node levelL _ _ _) (Node levelR _ _ _) = levelL <= levelR

insertB :: a -> Tree a -> Tree a
insertB value Leaf = Node 1 Leaf value Leaf
insertB value (Node level mtl v mtr) =
  if compareNodes mtl mtr
    then let newNode = insertB value mtl
          in Node (computeLevel newNode mtr) newNode v mtr
    else let newNode = insertB value mtr
          in Node (computeLevel newNode mtl) mtl v (insertB value mtr)

extractTreeLevel :: Tree a -> Integer
extractTreeLevel Leaf = 0
extractTreeLevel (Node level _ _ _) = level

computeLevel :: Tree a -> Tree a -> Integer
computeLevel mtl mtr = max (extractTreeLevel mtl) (extractTreeLevel mtr) + 1
