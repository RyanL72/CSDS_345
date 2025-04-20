
-- 1
interleave3 :: [a] -> [a] -> [a] -> [a]
interleave3 [] [] [] = []
interleave3 (x:xs) [] [] = x : interleave3 xs [] []
interleave3 [] (y:ys) [] = y : interleave3 [] ys []
interleave3 [] [] (z:zs) = z : interleave3 [] [] zs
interleave3 (x:xs) (y:ys) [] = x : y : interleave3 xs ys []
interleave3 (x:xs) [] (z:zs) = x : z : interleave3 xs [] zs
interleave3 [] (y:ys) (z:zs) = y : z : interleave3 [] ys zs
interleave3 (x:xs) (y:ys) (z:zs) = x : y : z : interleave3 xs ys zs

-- 2
insertInOrder :: Ord a => a -> [a] -> [a]
insertInOrder x [] = [x]
insertInOrder x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insertInOrder x ys

-- 3
listmax :: Ord a => [a] -> a
listmax = foldl1 max

-- 4
removedups :: Eq a => [a] -> [a]
removedups = foldr step []
  where
    step x [] = [x]
    step x acc@(y:_)
      | x == y    = acc
      | otherwise = x : acc


-- 5
data NestedList = Element Int | SubList [NestedList]
  deriving (Show, Eq)

-- Computes the total sum of a NestedList
sumNested :: [NestedList] -> Int
sumNested = sum . map sumOne
  where
    sumOne (Element n) = n
    sumOne (SubList xs) = sumNested xs

-- Adds the sum of a list at the front, recursively processing sublists
partialsums :: [NestedList] -> [NestedList]
partialsums xs = Element (sumNested xs) : map process xs
  where
    process (Element n) = Element n
    process (SubList ys) = SubList (partialsums ys)

-- 6
data Tree a = Empty
            | Leaf a
            | InnerNode a (Tree a) (Tree a)
  deriving (Show, Eq)


removeMin :: Ord a => Tree a -> Tree a
removeMin Empty = Empty  -- optional edge case
removeMin (Leaf _) = Empty
removeMin (InnerNode _ Empty right) = right
removeMin (InnerNode val left right) = InnerNode val (removeMin left) right

-- 7

yourfunction :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
yourfunction x maybeList test =
  if test x
    then fmap (++ [x]) maybeList
    else Nothing

checklist :: [a] -> (a -> Bool) -> Maybe [a]
checklist xs test = foldl (\acc x -> yourfunction x acc test) (Just []) xs

-- 8
checkappend :: Maybe [a] -> Maybe [a] -> (a -> Bool) -> Maybe [a]
checkappend (Just xs) (Just ys) test =
  if all test xs
    then Just (xs ++ ys)
    else Nothing
checkappend _ _ _ = Nothing

-- 9
sum_of_maxes :: (Ord a, Num a) => [[a]] -> [[a]] -> Maybe [a]
sum_of_maxes xs ys
  | length xs /= length ys = Nothing
  | otherwise = sequence (zipWith maxSum xs ys)
  where
    maxSum :: (Ord a, Num a) => [a] -> [a] -> Maybe a
    maxSum a b = do
      maxA <- safeMaximum a
      maxB <- safeMaximum b
      return (maxA + maxB)

    safeMaximum :: Ord a => [a] -> Maybe a
    safeMaximum [] = Nothing
    safeMaximum zs = Just (maximum zs)


-- 10 
data List a = Null | Pair a (List a)
  deriving (Show, Eq)

lreturn :: a -> List a
lreturn x = Pair x Null

lappend :: List a -> List a -> List a
lappend Null ys = ys
lappend (Pair x xs) ys = Pair x (lappend xs ys)

lbind :: List a -> (a -> List b) -> List b
lbind Null _ = Null
lbind (Pair x xs) f = f x `lappend` lbind xs f

