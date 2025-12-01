
elemNum :: Eq a => a -> [a] -> a
elemNum x [] = 0
elemNum x xs | x 

unique :: Eq a => [a] -> [a]
unique [] = []
unique xs = unique' xs xs []
unique' xs [] acc = acc
unique' xs (y:ys) acc = if 

insert :: Ord a => a -> [a] -> [a]

insert x [] = [x]
insert x (y:ys) | x >= y = x:y:ys 
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]

isort [] = []
isort (x:xs) = insert x (isort (unique xs))
