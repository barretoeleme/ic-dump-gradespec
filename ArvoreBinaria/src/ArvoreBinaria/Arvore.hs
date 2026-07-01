module ArvoreBinaria.Arvore
    (ABB(..), searchABB, insertABB, removeABB, tamABB, hightABB, printABB) where

data ABB a
    = Empty
    | Node a (ABB a) (ABB a)
    deriving (Show, Eq, Ord)

searchABB :: Ord a => a -> ABB a -> Bool
searchABB _ Empty = False
searchABB value (Node root left right)
    | value == root = True
    | value < root  = searchABB value left
    | otherwise     = searchABB value right


insertABB :: Ord a => a -> ABB a -> ABB a
insertABB value Empty = Node value Empty Empty
insertABB value (Node root left right)
    | value < root  = Node root (insertABB value left) right
    | value > root  = Node root left (insertABB value right)
    | otherwise     = Node root left right


removeABB :: Ord a => a -> ABB a -> ABB a
removeABB _ Empty = Empty
removeABB value (Node root left right)
    | value < root  = Node root (removeABB value left) right
    | value > root  = Node root left (removeABB value right)
    | otherwise =
        case (left, right) of
            (Empty, Empty) -> Empty
            (Empty, _)     -> right
            (_, Empty)     -> left
            (_, _) ->
                let (successor, newRight) = removeSmallest right
                in Node successor left newRight


removeSmallest :: ABB a -> (a, ABB a)
removeSmallest (Node root Empty right) = (root, right)
removeSmallest (Node root left right) =
    let (smallest, newLeft) = removeSmallest left
    in (smallest, Node root newLeft right)
removeSmallest Empty =
    error "Empty tree"


tamABB :: ABB a -> Integer
tamABB Empty = 0
tamABB (Node _ left right) =
    1 + tamABB left + tamABB right


hightABB :: ABB a -> Integer
hightABB Empty = -1
hightABB (Node _ left right) =
    1 + max (hightABB left) (hightABB right)


printABB :: Show a => ABB a -> String
printABB Empty = ""
printABB (Node value left right) =
    printABB left ++ show value ++ " " ++ printABB right