-- pre: x >= 0, y > 0
-- post: remainder x y = x % y

remainder :: Int -> Int -> Int
remainder x y   | x < y     = x
                | otherwise = remainder (x-y) y

-- I.H. when n < N, remainder n y = n % y

-- case 1: N < y
--  remainder N y = N   [defn. of remainder]
--                = N % y [defn. of %]

-- case 2: N >= y
--  remainder N y = remainder (N-y) y   [defn. of remainder]
--                = (N-y) % y           [by I.H. as (N-y) < N as y <= N]
--                = N % y               [defn. of %]