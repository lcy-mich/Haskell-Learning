import Test.QuickCheck

square :: Integer -> Integer
square x = x*x 

pyth :: Integer -> Integer -> Integer
pyth a b = square a + square b 

prop_square :: Integer -> Bool 
prop_square x = square x >= 0

prop_squares :: Integer -> Integer -> Bool
prop_squares x y = square (x+y) == square x + square y + 2*x*y 

prop_pyth :: Integer -> Integer -> Bool
prop_pyth x y = square (x+y) == pyth x y + 2*x*y