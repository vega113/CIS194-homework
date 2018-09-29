module Week4.BinaryTreeSpec
  ( main
  , spec
  ) where

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
      compareNodes (Node 0 Leaf 'A' Leaf) (Node 0 Leaf 'A' Leaf) `shouldBe` True
    it "compares trees with Node and Node correctly node > leaf" $ do
      compareNodes (Node 1 Leaf 'A' Leaf) (Leaf) `shouldBe` True
    it "compares trees with Node and Node correctly node > node" $ do
      compareNodes (Node 1 Leaf 'A' Leaf) (Node 0 Leaf 'A' Leaf) `shouldBe` True
    it "compares trees with Node and Node correctly node < node" $ do
      compareNodes (Node 1 Leaf 'A' Leaf) (Node 2 Leaf 'A' Leaf) `shouldBe` False
    it "folds list empty list " $ do foldTree ([] :: [String]) `shouldBe` Leaf
    it "folds list with only A" $ do foldTree "A" `shouldBe` Node 0 Leaf 'A' Leaf
    it "folds list with AB" $ do foldTree "AB" `shouldBe` Node 1 (Node 0 Leaf 'A' Leaf) 'B' Leaf
--    it "computes the same value as fun1 2" $ do fun1 [2] `shouldBe` 0
--    it "computes the same value as fun1 3" $ do fun1 [3] `shouldBe` 1
--    it "computes the same value as fun1 4" $ do fun1 [3, 4] `shouldBe` 2
    it "the level should log 2 of number of elements" $ property $ \l -> extractTreeLevel(foldTreeStr l) ===  toInteger( round (logBase 2 (length l)))
