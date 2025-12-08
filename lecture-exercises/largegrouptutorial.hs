
-- higher order functions

getSquares :: [Int] -> [Int]
getSquares = map (\x -> x*x) -- point free, no final argument

getSumOfSquares :: [Int] -> Int
getSumOfSquares = sum . getSquares -- (foldr (+) 0) . sqList  pointfree definition, . is function composition

greaterThanZero :: [Int] -> Bool
greaterThanZero = (foldr (&&) True). (map (> 0))

minimumVal :: Ord a => (Integer -> a) -> Integer -> a
minimumVal a n = minimum $ map a [0..n]

eqF :: Eq a => (Integer -> a) -> Integer -> Bool
eqF f n = foldr (&&) True $ map ((==f(0)).f)[0..n]

greaterThanZeroF :: Num a => (Integer -> a) -> Integer -> Bool
greaterThanZeroF f n = foldr (&&) True $ map ((> 0) . f)[0..n]

checkIncreasing :: Ord a => [a] -> Bool
checkIncreasing xs = foldr (&&) True $ map (\(x,y) -> x <= y) $ zip xs (tail xs)
