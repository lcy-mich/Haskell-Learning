import qualified Control.Applicative as called
-- proof by induction on recursive functions

-- correctness
-- pre condition and post condition logical statements hold on data before or after function applied (inputs/ outputs) 
-- (floyd hoare logic)

-- pre condition, x, y >= 0
-- post condition mult x y = x*y

mult :: Int -> Int -> Int
mult x 0 = 0
mult x y | even x = 2 * mult x (y `div` 2)
    | otherwise = x + 2*mult x (y `div` 2)

-- pre condition n > 0
-- post condition sumTo n = sum (1 to n)

sumTo :: Int -> Int
sumTo 1 = 1

-- recursive variant
sumTo n = n + sumTo (n - 1)

-- recursive funcs are shown correct by demonstrating two things
-- recursive variant exists
-- proof by induction that the function establishes the post condition

-- the recursive variant is a non negative expression:
-- using the input parameters which gets "smaller" each time the function is called.
-- input space is reduced to show function terminates
-- remember precondition has to be true (implicit guarantee of termination)

-- simple induction and course of values induction
-- (weak)                     (strong)
-- noetherian chain (descends)


-- COURSE OF VALUES INDUCTION ON MULT
-- IH assume mult x m = x * m for every m < N where N >= 0
-- show mult x N = x* N

-- two cases
-- A:
-- N is even

-- two sub cases:
-- N = 0: mult x 0 = 0 = x*0                [defn. of mult]
-- N > 0: mult x N = 2* mult x (N `div` 2)  [defn. of mult]
--                 = 2 * (mult x N/2)       [as N%2 = 0]
--                 = 2* x * N/2             [by I.H. as 0 < N/2 < N]
--                 = x * N                  

-- B:
-- N is odd

-- mult x N = x + 2* (mult x (N `div` 2 ))  [defn. of mult]
--          = x + 2 * mult x (N-1)/2        [as N % 2 = 1]
--          = x + 2 * x * (N-1)/2           [by I.H. as 0 < (N-1)/2 < N]
--          = x + x * (N-1)
--          = x*N
