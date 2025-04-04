-- Define Vector Type
data Vector = Vector Double Double
    deriving(Eq, Show)

-- Custom infix operator for the dot product
(@) :: Vector -> Vector -> Double
(Vector x1 y1) @ (Vector x2 y2) = x1*x2 + y1*y2

instance Num Vector where
    -- Vector Addition
    (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)