isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = if x == reverse x
    then True
    else False