main = interact isPalindrome

isPalindrome :: String -> String
isPalindrome = unlines . map (\x -> if checkPalindrome x then "Palindrome" else "Not Palindrome") . lines

checkPalindrome :: String -> Bool
checkPalindrome xs = xs == reverse xs

