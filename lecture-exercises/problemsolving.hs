import Prelude hiding (all)
-- problem solving functional style

-- sometimes u wanna fold a list from the left

-- foldl (op) z [] = z
-- foldl (op) z (x:xs) = foldl (op) (z op x) xs


-- reverse polish notation
-- 3 * (8 - 5) + 7
-- 3 8 5 - * 7 +

evalRPN :: (Num a, Read a) => String -> a
evalRPN = head . foldl procStack [] . words

procStack :: (Num a, Read a) => [a] -> String -> [a]

procStack (x:y:ys) "*" = (y*x) : ys
procStack (x:y:ys) "+" = (y+x) : ys
procStack (x:y:ys) "-" = (y-x) : ys
procStack xs numString = read numString : xs

-- matrices

type Matrix = [[Int]]

-- invariants
-- list in list has same legnth
-- at least one row and one column

-- 1. map length over list, check every num is same
-- 1. list is non-empty

all :: (a->Bool) -> [a] -> Bool
all p = foldr (&&) True . map p
-- all is a library function

uniform :: [Int] -> Bool
uniform [] = True -- vacuously true
uniform xs = all (== head xs) (tail xs)

-- check two properties
valid :: Matrix -> Bool
valid [] = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))


-- matrix addition, both has to be same height
-- check if conformable for addition (same width and same height)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f x y | (x,y) <- zip xs ys]

-- check conformability
-- check width
matrixWidth :: Matrix -> Int
matrixWidth xss = length (head xss)

-- check height
matrixHeight :: Matrix -> Int
matrixHeight xss = length xss

plusM :: Matrix -> Matrix -> Matrix
plusM m n | ok = zipWith (zipWith (+)) m n
    where ok = valid m && valid n
            && matrixWidth n == matrixWidth m
            && matrixHeight n == matrixHeight m