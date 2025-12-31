module Shapes
(
    Point(..)
,   Shape(..)
,   area
,   nudge
,   baseCircle
,   baseRect
) where
import Prelude hiding (fmap)

data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float
    deriving (Show)

area' :: Shape' -> Float
area' (Circle' _ _ r) = pi * r ^ 2
area' (Rectangle' x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person {
    firstName   ::  String,
    lastName    ::  String,
    age         ::  Int,
    height      ::  Float,
    phoneNumber ::  String,
    flavor      ::  String
} deriving (Show)

data IntMaybe = INothing | IJust Int
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | ShJust Shape

data Car' a b c = Car' { company' :: a
                     , model' :: b
                     , year' :: Int
                     } deriving (Show)

data Car = Car { company :: String,
                 model :: String,
                 year :: Int
                } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a  = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k ) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` l = Vector (i*l) (j*l) (k*l)

type Str = String

data List' a = Empty' | Cons' a (List' a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x > a = treeElem x right
    | x < a = treeElem x left

class Eq' a where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    x === y = not (x /== y)
    x /== y = not (x === y)

data TrafficLight = Red | Yellow | Green
instance Eq' TrafficLight where
    Red === Red = True
    Green === Green = True
    Yellow === Yellow = True
    _ === _ = False
instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow Light"
    show Green = "Green Light"

instance (Eq' m) => Eq' (Maybe m) where
    (===) :: Eq' m => Maybe m -> Maybe m -> Bool
    Just x === Just y = x === y
    Nothing === Nothing = True
    _ === _ = False

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo String where
    yesno "" = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
        then yesResult
        else noResult
        
class Functor' f where
    fmap :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap = map

instance Functor' Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ Nothing = Nothing

instance Functor' Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Functor' (Either a) where
    fmap :: (a2 -> b) -> Either a1 a2 -> Either a1 b
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

