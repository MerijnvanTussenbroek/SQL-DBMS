module Main where

import Library.Database
import Terminal (terminalEntrance) 

main :: IO ()
main = do
    terminalEntrance EmptyDB 
    putStrLn "Quitting Program"

