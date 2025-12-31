lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!!!"
lucky x = "Sorry, you're outta luck pal.."

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName _ = "NOT FOUND"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "NOTHING IN LIST"
head' (x:_) = x 

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "Theres one element which is: "++ show x
tell (x:y:[]) = "Theres two elements which is: "++ show x ++ " and " ++ show y
tell (x:y:_) = "There are many elements in this list. The first element is: "++show x++" and the second is: "++show y

badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x+y+z

firstLetter :: String -> String
firstLetter "" = "Empty string!!"
firstLetter og@(x:xs) = "The first letter of: " ++ og ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You underweight!"
    | bmi <= 25.0 = "Average weight!"
    | bmi <= 30.0 = "Overweight!"
    | otherwise = "Obese!"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight/ height ^ 2 <= 18.5 = "Underweight!"
    | weight/ height ^ 2 <= 25.0 = "Average weight!"
    | weight/ height ^ 2 <= 30.0 = "Overweight!"
    | otherwise = "Obese!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a >= b = a
    | otherwise = b 

compare' :: (Ord a) => a -> a -> Ordering 
a `compare'` b
    | a > b = GT 
    | a == b = EQ 
    | otherwise = LT 

bmiTell'' :: Double -> Double -> String 
bmiTell'' weight height
    | bmi <= skinny = "You underweight!"
    | bmi <= normal = "Average weight!"
    | bmi <= fat = "Overweight!"
    | otherwise = "Obese!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 35.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++[l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2*topArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^2]

head'' :: [a] -> a 
head'' xs = case xs of [] -> error "EMPTY LIST!!"
                       (x:_) -> x 

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton."
                                               xs -> "a longer list."
                                               
                        