import Prelude hiding (putStrLn, putStr, getLine)

-- prior to this, all code we've written are batch programs (takes input, gives outputs)
-- programs are now more interactive, has i/o even when the program is running
-- haskell programs are pure mathematical functions, they have no side effects
-- interactive programs have side effects like reading from keyboard and writing to screen
-- the fix is to use types to distinguish pure expressions from impure actions that involve side effects

-- IO Char -- type of actions that return a single character
-- IO -- () type of purely side effecting actions that have no result value ( () is the type of tuples with nothing)

-- primitive actions
-- getChar :: IO Char
-- reads char from keyboard, echoes to screen, returns character as result value
-- putChar :: Char -> IO ()
--  writes character to screen, returning no result value
-- return :: a -> IO a
-- returns value without performing any interaction

-- sequencing actions
-- as a single composite actions using keyword do
getTwo :: IO (Char, Char)
getTwo = do x <- getChar -- rename result of getChar to x and y
            y <- getChar
            return (x,y)

getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then return []
             else do xs <- getLine
                     return (x:xs)
                     
putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do putChar x
                   putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string \n"
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

-- bro just took 10 minutes explaining that to put a string on the screen, you need to take the integer and convert it to a string to show on the screen cus the screen needs a string to be shown using putStr which takes a string and not a number

swapAround = do line <- getLine
                if null line then return ()
                else do putStrLn $ reverseWords line

reverseWords :: String -> String
reverseWords = unwords . map reverse . words 
-- words :: String -> [String]
-- unwords :: [String] -> String