module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import ArvoreBinaria.Arvore

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Binary Search Tree Tests"
    [ testSearch
    , testInsert
    , testRemove
    , testSize
    , testHeight
    , testPrint
    ]

testSearch :: TestTree
testSearch = testCase "Searching values" $ do
    let tree =
            insertABB 10 $
            insertABB 5 $
            insertABB 15 $
            insertABB 8 Empty

    searchABB 10 tree @?= True
    searchABB 8 tree @?= True
    searchABB 20 tree @?= False


testInsert :: TestTree
testInsert = testCase "Insertion" $ do
    let tree =
            insertABB 10 $
            insertABB 5 $
            insertABB 15 Empty

    tamABB tree @?= 3


testRemove :: TestTree
testRemove = testCase "Removing elements" $ do
    let tree =
            insertABB 10 $
            insertABB 5 $
            insertABB 15 Empty

    let newTree = removeABB 5 tree

    searchABB 5 newTree @?= False
    tamABB newTree @?= 2


testSize :: TestTree
testSize = testCase "Tree size" $ do
    let tree =
            insertABB 10 $
            insertABB 5 $
            insertABB 15 $
            insertABB 8 Empty

    tamABB tree @?= 4


testHeight :: TestTree
testHeight = testCase "Tree height" $ do
    let tree =
            insertABB 10 $
            insertABB 5 $
            insertABB 15 $
            insertABB 8 Empty

    hightABB tree @?= 2


testPrint :: TestTree
testPrint = testCase "In-order traversal" $ do
    let tree =
            insertABB 10 $
            insertABB 5 $
            insertABB 15 $
            insertABB 8 Empty

    printABB tree @?= "5 8 10 15 "