import Control.Monad
main = do
    contents <- getContents
    putStr $ shortlines contents
    
    where
        shortlines = unlines . filter (\line -> length line < 10) . lines
        