module Main where

import Library.Environment
import Terminal (terminalEntrance)

main :: IO ()
main = do
    terminalEntrance EmptyDB
    putStrLn "Quitting Program"

