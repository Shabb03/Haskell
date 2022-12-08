data BinTree t = Empty | Root Int (BinTree t) (BinTree t)
    deriving (Eq, Ord, Show)


leaf x = Root x Empty Empty


addNode :: Int -> BinTree t -> BinTree t
addNode a Empty = leaf a
addNode x (Root a left right)
 | x < a = Root a (addNode x left) right
 | otherwise = Root a left (addNode x right)


makeTree :: [Int] -> BinTree t
makeTree [] = Empty
makeTree [x] = leaf x
makeTree (x:xs) = addNode x (makeTree xs)


inorder :: BinTree t -> [Int]
inorder Empty = []
inorder (Root x left right) = inorder left ++ [x] ++ inorder right