

import Test.Hspec
import Test.QuickCheck

import Week4.BinaryTree

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "balanced binary tree'" $ do
    it "compares trees with Node and Node correctly left level == right level" $ do
      compareNodes (Node 0 Leaf 'A' Leaf) (Node 1 Leaf 'A' Leaf) `shouldBe` True

    it "compares trees with Node and Node correctly node > leaf" $ do
      compareNodes (Node 1 Leaf 'A' Leaf) (Leaf) `shouldBe` False

    it "compares trees with Node and Node correctly node > node" $ do
      compareNodes (Node 1 Leaf 'A' Leaf) (Node 1 Leaf 'A' Leaf) `shouldBe` True

    it "compares trees with Node and Node correctly node < node" $ do
      compareNodes (Node 1 Leaf 'A' Leaf) (Node 2 Leaf 'A' Leaf) `shouldBe` True

    it "folds list empty list " $ do foldTree ([] :: [String]) `shouldBe` Leaf

    it "folds list with only A" $ do foldTree "A" `shouldBe` Node 1 Leaf 'A' Leaf

    it "folds list with AB" $ do foldTree "AB" `shouldBe` Node 2 Leaf 'B' (Node 1 Leaf 'A' Leaf)

    it "passes is blanced tree check" $ property $ \l -> verifyBalancedTree(foldTreeStr l) === True

    it "level of left sub tree should not differ by more than 1 from right sub tree" $
      property $ \l ->
        let treeA = foldTreeStr l
            childrenLevels = extractChildrenLevels treeA
         in case childrenLevels of
              (levelChildL, levelChildR) -> (abs (levelChildL - levelChildR) <= 1) === True

verifyBalancedTree :: Tree a -> Bool
verifyBalancedTree Leaf = True
verifyBalancedTree (Node _ mtl _ mtr) = abs ((extractTreeLevel mtl) - (extractTreeLevel mtr)) <= 1 && (verifyBalancedTree mtl) && (verifyBalancedTree mtr)

foldTreeStr :: String -> Tree Char
foldTreeStr = foldTree

extractChildrenLevels :: Tree a -> (Integer, Integer)
extractChildrenLevels Leaf = (0,0)
extractChildrenLevels (Node _ mtl _ mtr) = (extractTreeLevel mtl, extractTreeLevel mtr)