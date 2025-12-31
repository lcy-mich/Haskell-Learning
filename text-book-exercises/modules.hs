import Data.List (nub, words, group, sort, tails, isPrefixOf, find)
-- import qualified Data.Map as M
import Data.Char (ord, chr, digitToInt, isDigit) 

numUniques :: (Eq a) => [a] -> Int 
numUniques = length . nub 

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

hasSubstring :: Eq a => [a] -> [a] -> Bool
hasSubstring xs ys = foldr (||) False $ map (xs `isPrefixOf`) (tails ys)

isIn :: (Eq a) => [a] -> [a] -> Bool 
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- caesar cipher

encode :: Int -> String -> String
encode offset = map (\x -> chr $ offset + ord x)

decode :: Int -> String -> String
decode offset = encode (negate offset)

-- cool numbers

digitsSum :: Int -> Int
digitsSum = sum . map digitToInt . show

firstTo40 :: Maybe Int 
firstTo40 = find (\x -> digitsSum x == 40) [1..]

-- key value datastructs (association lists)

findKey :: Eq k => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs 

findKey' :: Eq k => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing 
findKey' key ((k, v):xs)
    | key == k = Just v
    | otherwise = findKey' key xs

findKey'' :: Eq k => k -> [(k, v)] -> Maybe v
findKey'' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

--  phone book
string2digits :: String -> [Int] 
string2digits ds = [digitToInt d | d <- ds, isDigit d]

string2digits'' :: String -> [Int]
string2digits'' = map digitToInt . filter isDigit

import Geometry