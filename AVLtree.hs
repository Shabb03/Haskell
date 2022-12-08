--CA320 Assignment
--
--Rishabdev Sidhu
--20309616

--Define the AVL Tree
data AVLtree t = Empty | Root t (AVLtree t) (AVLtree t)
    deriving (Eq, Ord, Show)

leaf x = Root x Empty Empty

--Get the height of the tree
height :: AVLtree a -> Int
height Empty = 0
height (Root _ left right) = 1 + (max (height left) (height right))

--Check whether the tree is left heavy or right heavy
balance :: AVLtree a -> Int
balance Empty = 0
balance (Root _ left right) = (height left) - (height right)

--Get the root value of the tree
rootVal :: AVLtree a -> a
rootVal Empty = error "Empty Root"
rootVal (Root a _ _) = a

--Return the left subtree
leftTree :: AVLtree a -> AVLtree a
leftTree Empty = Empty
leftTree (Root _ left _) = left

--Return the right subtree
rightTree :: AVLtree a -> AVLtree a
rightTree Empty = Empty
rightTree (Root _ _ right) = right

--Rotate the tree left or right appropriately if the height difference between the left and right subtree is greater than 1
rotate :: AVLtree a -> AVLtree a
rotate Empty = Empty
rotate (Root n Empty Empty) = (Root n Empty Empty)
rotate (Root n left right)
--LL Rotation
--If the left subtree is heavier than the right and the furthest child is on the left
    | (balance (Root n left right)) > 1 && (balance left) == 1 = (Root (rootVal left) (leftTree left) (Root n (rightTree left) right))
--RR Rotation
--If the right subtree is heavier than the left and the furthest child is on the right
    | (balance (Root n left right)) < -1 && (balance right) == -1 = (Root (rootVal right) (Root n left (leftTree right)) (rightTree right))
--LR Rotation
--If the left subtree is heavier than the right and the furthest child is on the right of that subtree
    | (balance (Root n left right)) > 1 && (balance left) == -1 = (Root (rootVal (rightTree left)) 
                                                                  (Root (rootVal left) (leftTree left) (leftTree (rightTree left)))
                                                                  (Root n (rightTree (rightTree left)) right))
--RL Rotation
--If the right subtree is heavier than the left and the furthest child is on the left of that subtree
    | (balance (Root n left right)) < -1 && (balance right) == 1 = (Root (rootVal (leftTree right)) 
                                                                   (Root n left (leftTree (leftTree right))) 
                                                                   (Root (rootVal right) (rightTree (leftTree right)) (rightTree right)))
--No Rotation
    | otherwise = (Root n left right)

--Insert a value into the subtree and constantly rotate the tree to maintain its AVL properties
addNode :: (Ord a) => a -> AVLtree a -> AVLtree a
addNode x Empty = leaf x
addNode x (Root n left right)
    | x < n = rotate (Root n (addNode x left) right)
    | otherwise = rotate (Root n left (addNode x right))

--Make an AVL Tree given a list of integers
makeTree :: (Ord a) => [a] -> AVLtree a
makeTree [] = Empty
makeTree (x:xs) = addNode x (makeTree xs)

--Return a list of values from the AVL Tree from lowest to highest, used for debugging purposes
----inorder :: AVLtree a -> [a]
----inorder Empty = []
----inorder (Root x left right) = inorder left ++ [x] ++ inorder right

--Return a 3 * n number of spaces
spaces :: Int -> String
spaces 0 = ""
spaces 1 = ""
spaces n = "   " ++ spaces (n-1)

--Function to return the tree as a string using inorder traversal with the tree being sideways
prettyPrint2 :: (Show a) => AVLtree a -> String
prettyPrint2 Empty = ""
prettyPrint2 (Root x Empty Empty) = (show x) ++ "\n"
prettyPrint2 (Root x left right) = prettyPrint2 left ++ (show (spaces h)) ++ (show x) ++ "\n" ++ prettyPrint2 right
    where h = (height (Root x left right))

--Function to display the tree by outputting onto the GHCI
prettyPrint :: (Show a) => AVLtree a -> IO ()
prettyPrint (Root x left right) = putStrLn (prettyPrint2 (Root x left right))