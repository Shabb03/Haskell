validTriangle :: (RealFloat a) => a -> a-> a-> Bool
validTriangle x y z
    | x+y < z = False
    | x+z < y = False
    | y+z < x = False
    | otherwise = True


triangleArea :: (RealFloat a) => a -> a-> a -> a
triangleArea a b c = if (validTriangle a b c)
    then let s = (a+b+c)/2
    in sqrt (s * (s-a) * (s-b) * (s-c))
    else error "Not a triangle!"