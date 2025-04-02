{- replace all occurrences of an element with another, using guards -}
replaceall _ _ [] = []
replaceall a b (h:t)
    | h == a    = b : replaceall a b t
    | otherwise = h : replaceall a b t

{- merge two sorted lists into one sorted list, using guards -}
merge [] l = l
merge l [] = l
merge (h1:t1) (h2:t2)
    | h1 < h2   = h1 : merge t1 (h2:t2)
    | h1 > h2   = h2 : merge (h1:t1) t2
    | otherwise = h1 : h2 : merge t1 t2

{- compute the length of a list using pattern matching and function composition -}
len :: [a] -> Int
len [] = 0
len (_:t) = (1 +) . len $ t
