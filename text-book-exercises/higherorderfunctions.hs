multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

multTwoWithNine :: Int -> Int -> Int
multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

divideByTen :: Floating a => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs)
    | pred x = x : filter' pred xs
    | otherwise = filter' pred xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
    where smaller = filter (<=x) xs
          larger = filter (>x) xs

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

collatzchain :: Integer -> [Integer]
collatzchain 1 = [1]
collatzchain n
    | even n = n : collatzchain (n `div` 2)
    | otherwise = n : collatzchain (3*n + 1)

largerThanFifteen :: Int
largerThanFifteen = length (filter isLong (map length chainlist))
    where chainlist = map collatzchain [1..1000]
          isLong = (>= 15)

addThree :: Int -> Int -> Int -> Int
addThree = \x -> \y -> \z -> x + y + z

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

elem' :: Eq a => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum' :: Ord a => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: Num a => [a] -> a
product' = foldl (*) 1

filter'' :: (a->Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' = foldr (&&) True

squareroots :: [Double]
squareroots = map sqrt [1..]
sumUntil :: Int
sumUntil = length (takeWhile (<1000) (scanl1 (+) squareroots)) +1


