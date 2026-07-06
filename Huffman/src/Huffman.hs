module Huffman
(
    Huffman(..),
    frequency,
    makeLeaves,
    buildTree,
    codes,
    encode,
    decode
) where

import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)

data Huffman
    = Leaf Char Int
    | Node Int Huffman Huffman
    deriving (Show)

freq :: Huffman -> Int
freq (Leaf _ f) = f
freq (Node f _ _) = f

frequency :: String -> [(Char, Int)]
frequency str =
    Map.toList $
    Map.fromListWith (+) [(c,1) | c <- str]

makeLeaves :: [(Char, Int)] -> [Huffman]
makeLeaves =
    map (\(c,f) -> Leaf c f)

insertTree :: Huffman -> [Huffman] -> [Huffman]
insertTree t ts =
    sortBy (comparing freq) (t:ts)

buildTree :: [Huffman] -> Huffman
buildTree [t] = t
buildTree ts =
    buildTree (insertTree newTree rest)
    where
        sorted = sortBy (comparing freq) ts
        t1 = head sorted
        t2 = sorted !! 1
        rest = drop 2 sorted
        newTree = Node (freq t1 + freq t2) t1 t2

codes :: Huffman -> Map.Map Char String
codes tree =
    Map.fromList (generate tree "")
    where
        generate (Leaf c _) code = [(c, code)]
        generate (Node _ l r) code =
            generate l (code ++ "0") ++
            generate r (code ++ "1")

encode :: Map.Map Char String -> String -> String
encode table =
    concatMap (\c -> table Map.! c)

decode :: Huffman -> String -> String
decode tree bits =
    decodeAux tree bits tree

decodeAux :: Huffman -> String -> Huffman -> String
decodeAux _ [] (Leaf c _) = [c]
decodeAux _ [] _ = []
decodeAux tree xs (Leaf c _) =
    c : decodeAux tree xs tree
decodeAux tree (b:bs) (Node _ l r)
    | b == '0' = decodeAux tree bs l
    | otherwise = decodeAux tree bs r