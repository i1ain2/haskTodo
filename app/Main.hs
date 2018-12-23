{-
    A simple console based todo app.
-}

module Main where

import           System.IO
import           System.Directory
import           Data.List

{-
    Global variables
    TODO: グローバル変数使うの微妙では？？完全定数だからOKか？
-}
todoFileName = ".todo.txt"

main = do 
    view
    command <- cmd
    dispatch command


-- TODO: コマンド以外が入力された場合に、エラーになるようにする
dispatch :: String -> IO ()
dispatch "/view" = view
dispatch "/add" = add
dispatch "/complete" = complete


cmd :: IO String 
cmd = do 
    putStrLn "Please enter command ..."
    getLine


view :: IO ()
view = do
    contents <- readFile todoFileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " " ++ line) [0..] todoTasks
    putStrLn "Todo List"
    putStr $ unlines numberedTasks


add :: IO ()
add = do
    putStrLn "add task ..."
    todoItem <- getLine
    appendFile todoFileName ("- [ ] " ++ todoItem ++ "\n")


complete :: IO ()
complete = do
    contents <- readFile todoFileName
    let todoTasks = lines contents

    putStrLn "select complete task ..."
    numberString <- getLine
    -- TODO: todoを削除するのではなく、xを付与する
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile todoFileName
    renameFile tempName todoFileName