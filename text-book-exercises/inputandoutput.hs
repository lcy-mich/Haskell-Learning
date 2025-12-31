import Data.Char
import Control.Monad   
main = putStrLn "hello, world"

main1 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

main2 = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey" ++ bigFirstName ++ " "
                     ++ bigLastName  ++ " "
                     ++ ", how are you?"

main3 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main4

reverseWords :: String -> String
reverseWords = unwords . reverse . map reverse . words

main4 = do
    putStr "Hey, "
    putStr "I'm "
    putStrLn "Andy!"

main5 = do
    putChar 't'
    putChar 'e'
    putChar 'h'
    putChar '\n'
    putStr "hey! "

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' (x:xs) = do
    putChar x
    putStrLn' xs

main6 = do
    print True
    print 2
    print "haha"
    print 3.2
    print [3,4,3]

main7 = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

main8 = do
    -- a <- getLine
    -- b <- getLine
    -- c <- getLine
    -- print [a,b,c]
    rs <- sequence [getLine, getLine, getLine]
    print rs
    sequence $ map (putStr . show) [1,2,3,4,5]
    putChar '\n'
    return ()

main9 = do
    mapM_ print [1,2,3,4,5,6]

main10 = forever $ do
    putStr "Gimme sum input: "
    l <- getLine
    putStrLn $ map toUpper l

main11 = do
    colors <- forM [1..4] (\a -> do
        putStrLn $ "Which color do you associate with the number "
            ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM_ putStrLn (map show (zip [1..4] colors))
