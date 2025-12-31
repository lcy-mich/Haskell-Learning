import Data.Char (toUpper, toLower)

square :: Num a => a -> a 
square x = x^2
    
pyth :: Integral a => a -> a -> a 
pyth x y = (square x) + (square y)

isTriple :: Integral a => a -> a -> a -> Bool
isTriple x y z = (pyth x y) == (square z) 

isTripleAny :: Integral a => a -> a -> a -> Bool
isTripleAny x y z = foldr (||) False
    [isTriple a b c | a <- all, b <- all, c <- all] 
    where all = [x,y,z]

halfEvens :: [ Int ] -> [ Int ]
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange x y zs = [z | z <- zs, (z >= x) && (z <= y)]

countPositives :: [Int] -> Int
countPositives xs = sum [1 | x <- xs, x > 0]

capitalised :: String -> String
capitalised (c:cs) = toUpper c : [toLower d | d <- cs]

title :: [String] -> [String]
title [] = []
title (t:ts) = capitalised t : [if length g > 3 then capitalised g else lowerAll g | g <- ts]
    where
        lowerAll [] = []
        lowerAll cs = [toLower c | c <- cs]
