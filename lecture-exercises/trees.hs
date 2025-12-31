data Tree a = Leaf a | Node (Tree a) (Tree a)


balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node x y) = (z <= 1 && z >= (-1)) && (balanced x) && (balanced y)
    where z = (numLeaf x) - (numLeaf y)

numLeaf :: Tree a -> Int
numLeaf (Leaf x) = 1
numLeaf (Node x y) = (numLeaf x) + (numLeaf y)

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitHalf' xs []

splitHalf' :: [a] -> [a] -> ([a], [a])
splitHalf' [] ys = ([], ys)
splitHalf' (x:xs) ys = if (length (x:xs)) - (length ys) <= 1 && (length (x:xs)) - (length ys) >= -1 then ((x:xs), ys) else splitHalf' xs (x:ys)




balance :: [a] -> Tree a
balance (x:[]) = (Leaf x)
balance (x:(y:[])) = (Node (Leaf x) (Leaf y ))
balance zs = (Node (balance xs) (balance ys))
    where (xs, ys) = splitHalf zs
