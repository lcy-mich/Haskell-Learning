reverse' :: [a] -> [a]
reverse' xs = revInto [] xs
    where 
        revInto ys [] = ys
        revInto ys (x:xs) = revInto (x:ys) xs
-- use accumulating parameter
     