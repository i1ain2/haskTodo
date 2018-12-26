{-
    A simple console based todo app.
-}

module Main where

import           System.IO
import           System.Exit
import           System.Directory
import           Data.List

{-
    Global variables
    TODO: グローバル変数使うの微妙では？？完全定数だからOKか？
-}
todoFileName = ".todo.txt"

data Mode = Add | Complete deriving (Eq, Show, Read)


main = do
    putStrLn "haskTodo"
    accept Add


accept :: Mode -> IO ()
accept mode = do
    view
    contents <- getLine
    dispatch mode contents
    accept mode


dispatch :: Mode -> String -> IO ()
dispatch _        "/view"           = view
dispatch _        "/add"            = accept Add
dispatch _        "/complete"       = accept Complete
dispatch _        "/clear"          = clear
dispatch _        "/exit"           = exitSuccess
dispatch _        command@('/' : _) = doesntExist command
dispatch _        ""                = emptyString
dispatch Add      task              = add task
dispatch Complete number            = complete (read number)


doesntExist :: String -> IO ()
doesntExist command = putStrLn $ "The " ++ command ++ " command doesn't exist."


emptyString :: IO ()
emptyString = putStrLn "Empty. Input task or command."


cmd :: IO String
cmd = do
    putStrLn "Please enter command ..."
    getLine


view :: IO ()
view = do
    contents <- readFile todoFileName
    let todoTasks = lines contents
        numberedTasks =
            zipWith (\n line -> show n ++ " " ++ line) [0 ..] todoTasks
    putStrLn "Todo List"
    putStr $ unlines numberedTasks


add :: String -> IO ()
add todoItem = appendFile todoFileName ("- [ ] " ++ todoItem ++ "\n")


complete :: Int -> IO ()
complete number = do
    contents <- readFile todoFileName
    let todoTasks = lines contents

    let completed    = "- [x] " ++ drop 6 (todoTasks !! number)
        newTodoItems = unlines $ replaceAt number completed todoTasks
    updateFile newTodoItems


clear :: IO ()
clear = do
    contents <- readFile todoFileName
    let newTodoItems =
            unlines $ filter (\x -> take 5 x /= "- [x]") (lines contents)
    updateFile newTodoItems


updateFile :: String -> IO ()
updateFile newTodoItems = do
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile todoFileName
    renameFile tempName todoFileName


replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = (take n xs) ++ [x] ++ (drop (n + 1) xs)
