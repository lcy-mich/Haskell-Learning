import Prelude hiding (reverse)
-- testing programs with property oracles

-- example with reverse
-- properties of reverse

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- maths

-- reverse [x] = [x]
-- reverse (xs ++ ys) = reverse ys ++ reverse xs 
-- reverse (reverse xs) = xs


-- formalise properties into predicates
prop_RevUnit :: Eq a => a->Bool
prop_RevUnit x = reverse' [x] == [x]

prop_RevApp :: Eq a => [a] -> [a] -> Bool
prop_RevApp xs ys = reverse' (xs ++ ys) == reverse' ys ++ reverse' xs

prop_RevRev :: Eq a => [a] -> Bool
prop_RevRev xs = reverse' (reverse' xs) == xs
