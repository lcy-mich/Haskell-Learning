import Control.Monad
main = interact shortlines

shortlines :: String -> String
shortlines = unlines . filter (\line -> length line < 10) . lines

