fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

maximum' :: Ord a => [a] -> a 
maximum' [] = error "empty list for maximum"
maximum' [x] = x 
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' 1 val = [val]
replicate' n val = val : replicate' (n-1) val

replicate'' :: Int -> a -> [a]
replicate'' n x
    | n <= 0 = []
    | otherwise = x : replicate'' (n-1) x

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : (take' (n-1) xs)

take'' :: Int -> [a] -> [a]
take'' n xs
    | null xs || n <= 0 = []
    | otherwise = let (z:zs) = xs in z: take'' (n-1) zs

reverse' :: [a] -> [a]
reverse' (x:xs)
    | null (x:xs) = []
    | otherwise = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
_ `elem'` [] = False
x `elem'` (y:ys) = (x == y) || (x `elem'` ys)

elem'' :: Eq a => a -> [a] -> Bool 
_ `elem''` [] = False
a `elem''` (x:xs)
    | a == x    = True
    | otherwise = a `elem''` xs

-- quicksort

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort [y | y <- xs, y <= x]) ++ [x] ++ (quicksort [y | y <- xs, y >= x])
