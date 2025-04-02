-- this is a single line comment

{-
 This is a 
 multiline comment
-}

{- write append three ways, first the "normal way -}
myappend1 l1 l2 =
    if l1 == []
        then 
            l2
        else
            -- (cons (car l1) (myappend (cdr l1))
            (head l1) : myappend1 (tail l1) l2
                
{- append using a lambda -}
myappend2 :: (Eq a) =>[a] -> [a] -> [a]
myappend2 =
    \l1 l2 ->
        if l1 == []
            then 
                l2
            else
                (head l1) : myappend2 (tail l1) l2

{- append using pattern matching -}
myappend3 [] l = l
myappend3 (h:t) l = h : myappend3 t l