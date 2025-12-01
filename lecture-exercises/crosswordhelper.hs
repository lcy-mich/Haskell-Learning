-- list comprehension

cwordFind :: Char -> Int -> Int -> [String] -> [String]
cwordFind letter pos len words = [wd|wd <- words, length wd == len, wd !! pos == letter]

-- higher order

cwordFindH0 :: Char -> Int -> Int -> [String] -> [String]
cwordFindH0 letter pos len words = filter p words
    where p x = (x !! pos == letter) && (length x == len)
