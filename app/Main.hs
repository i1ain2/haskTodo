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


dispatch :: String -> IO ()
dispatch "/view"     = view
dispatch "/add"      = add
dispatch "/complete" = complete
dispatch "/clear"    = clear
dispatch command     = doesntExist command


doesntExist :: String -> IO ()
doesntExist command = putStrLn $ "The " ++ command ++ " command doesn't exist"


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
    let number       = read numberString
        completed    = "- [x] " ++ drop 6 (todoTasks !! number)
        newTodoItems = unlines $ replaceAt number completed todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile todoFileName
    renameFile tempName todoFileName


clear :: IO ()
clear = do
    contents <- readFile todoFileName
    let newTodoItems = unlines $ filter (\x -> take 5 x /= "- [x]") (lines contents)
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile todoFileName
    renameFile tempName todoFileName


replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = (take n xs) ++ [x] ++ (drop (n + 1) xs)
