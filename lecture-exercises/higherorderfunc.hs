f :: [Int] -> Int
f xs = foldr (+) 0 (map sqr (filter pos xs))
    where
        sqr x = x*x    
        pos x = x>0


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

sum' = foldr1 (+)