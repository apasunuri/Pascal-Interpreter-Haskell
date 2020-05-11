module Main where

import Pascal
import System.Environment

main :: IO ()
main = do
    (fileName:_) <- System.Environment.getArgs
    contents <- readFile fileName
    case parseString contents of 
        Left err -> print $ show err
        Right ast -> putStrLn $ interpret ast 

