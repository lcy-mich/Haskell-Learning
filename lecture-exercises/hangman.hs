{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
-- one player secretly enters a word
-- another player works out what word by guessing it
-- each guess represents which letters in secret word occurs in the guess

-- game ends when guess is correct, or guessing player guess too much (9 guesses)


hangman :: IO ()
hangman = do    putStrLn "Think of a word:"
                word <- sgetLine
                putStrLn "Try to guess it"
                play word
                -- now to define secret getline and play

sgetLine :: IO String
sgetLine =  do  x <- getCh
                if x == '\n' then
                    do  putChar x
                        return []
                else do putChar '-'
                        xs <- sgetLine
                        return (x:xs)

getCh :: IO Char
getCh = do  hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x
                        
play :: String -> IO ()
play word = do  putStr "?"
                guess <- getLine
                if guess == word then
                    putStrLn "You Got It!"
                else
                    do  putStrLn (match word guess)
                        play word

match :: String -> String -> String
match xs ys = [ if elem x ys then x else '-' | x <- xs]
