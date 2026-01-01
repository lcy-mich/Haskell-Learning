import System.IO
import Control.Monad

main = forever $ do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
