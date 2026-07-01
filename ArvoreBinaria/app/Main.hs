module Main where

import ArvoreBinaria.Arvore

main :: IO ()
main = do
    let tree0 = Empty
    let tree1 = insertABB 10 tree0
    let tree2 = insertABB 5 tree1
    let tree3 = insertABB 15 tree2
    let tree4 = insertABB 8 tree3

    putStrLn "Tree (in-order):"
    putStrLn (printABB tree4)

    putStrLn ("Size: " ++ show (tamABB tree4))
    putStrLn ("Height: " ++ show (hightABB tree4))

    putStrLn ("Search 8: " ++ show (searchABB 8 tree4))
    putStrLn ("Search 20: " ++ show (searchABB 20 tree4))