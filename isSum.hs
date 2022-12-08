isSum :: Int -> Int -> Int -> Bool
isSum x y z
    | x+y == z = True
    | x+z == y = True
    | y+z == x = True
    | otherwise = False