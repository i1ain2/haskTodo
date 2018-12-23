{-
    A simple console based todo app.
-}

module Main where

import           System.IO
import           System.Directory
import           Data.List

main = do 
    view ".todo.txt"
    command <- cmd
    dispatch command


-- TODO: コマンド以外が入力された場合に、エラーになるようにする
dispatch :: String -> IO ()
dispatch "/view" = view ".todo.txt"
dispatch "/add" = add ".todo.txt"
dispatch "/complete" = complete ".todo.txt" 


cmd :: IO String 
cmd = do 
    putStrLn "Please enter command ..."
    getLine


view :: String -> IO ()
view fileName = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " " ++ line) [0..] todoTasks
    putStrLn "Todo List"
    putStr $ unlines numberedTasks


add :: String -> IO ()
add fileName = do
    putStrLn "add task ..."
    todoItem <- getLine
    appendFile fileName ("- [ ] " ++ todoItem ++ "\n")


complete :: String -> IO ()
complete fileName = do
    contents <- readFile fileName
    let todoTasks = lines contents

    putStrLn "select complete task ..."
    numberString <- getLine
    -- TODO: todoを削除するのではなく、xを付与する
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName