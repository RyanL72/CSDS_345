{- square each element using if/then -}
squares1 :: (Num a, Eq a) => [a] -> [a]
squares1 l =
    if l == []
        then
            []
        else
            (head l)^2 : squares1 (tail l)

{- square each element using a lambda function -}
squares2 :: (Num a, Eq a) => [a] -> [a]
squares2 =
    \l ->
        if l == []
            then
                []
            else
                (head l)^2 : squares2 (tail l)

squares3 :: (Num a) => [a] -> [a]
squares3 [] = []
squares3 (h:t) = (h^2) : squares3 t
