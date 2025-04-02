{- reverse a list -}
myreverse1 [] = []
myreverse1 (h:t) = (myreverse1 t) ++ [h]

{- reverse a list using function composition -}
myreverse2 [] = []
myreverse2 (h:t) = ((++) . myreverse2) t [h]

{- remove all copies from an elmeent from a list -}
removeall a [] = []
removeall a (h:t) =
    if h == a
        then
            removeall a t
        else
            h : removeall a t

{- removeall using pattern matching and a "guard" -}
removeall2 _ [] = []
removeall2 a (h:t)
    | a == h    = removeall a t
    | otherwise = h : removeall a t