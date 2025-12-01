import Prelude hiding (Just, Maybe)
data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y


-- data Maybe a = Nothing | Just a
-- return :: a -> Maybe a
-- return x = Just x

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Nothing >>= _ = Nothing
-- Just x >>= f = f x

data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

--arithmetic expressions

data Expr = Val Int
    | Add Expr Expr
    | Mul Expr Expr

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y