{-
Write a Haskell function squares that takes a list of numbers are returns a list where each number is squared.

*Main> squares [3,5,7,9]
[9,25,49,81]
Write this function three ways:

As a "normal" if/then statement
As a "lambda" function bound to the name
Using pattern matching
-}

-- Normal way
squares1 l1 =
    if l1 == []
        then 
            l1
        else
            (head l1) * (head l1) : squares1((tail l1))

-- lambda
squares2:: (Eq a, Num a) => [a] -> [a]
squares2 =
    \l1 ->
        if l1 == []
            then 
                []
            else
                ((head l1) * (head l1)) : squares2((tail l1))

-- pattern matching
squares3 [] = []
squares3 (h:t) = (h * h) : squares3 t
