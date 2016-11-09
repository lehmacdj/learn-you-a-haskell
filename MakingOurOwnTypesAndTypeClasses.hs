data Point = Point Float Float
    deriving (Show)

data Shape =
      Circle Point Float
    | Rectangle Point Point
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

makeCircle :: Point -> Float -> Shape
makeCircle = Circle

nudgePoint :: Point -> Float -> Float -> Point
nudgePoint (Point x y) dx dy = Point (x + dx) (y + dy)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle p r) x y = Circle (nudgePoint p x y) r
nudge (Rectangle p1 p2) x y =  Rectangle (nudgePoint p1 x y) (nudgePoint p2 x y)

unitCircle = Circle (Point 0 0) 1
genericRectangle = Rectangle (Point 0 0) (Point 3 4)
unitSquare = Rectangle (Point 0 0) (Point 1 1)

-- Record syntax
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- Type variables
data Vector a = Vector a a a
    deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1 + y1) (y1 + y2) (z1 + z2)

vectMult :: (Num a) => Vector a -> a -> Vector a
(Vector x y z) `vectMult` c = Vector (x * c) (y * c) (z * c)

dotMult :: (Num a) => Vector a -> Vector a -> a
(Vector x1 y1 z1) `dotMult` (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)
singleton :: a -> Tree a
singleton x = Node x Leaf Leaf

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Leaf = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Leaf = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node v r l) = Node (f v) (fmap f r) (fmap f l)

-- Either is like Result type in OCaml
-- Right : Ok :: Left : Error

class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a}
    deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
