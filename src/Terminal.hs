module Terminal where

import IO
import Parser
import Library.Environment

terminalEntrance :: Database ->  IO ()
terminalEntrance db = do
                input <- getLine
                terminalMaintenance db input

terminalMaintenance :: Database -> String -> IO ()
terminalMaintenance db x    | x == "exit" = terminalTermination
                            | otherwise = do
                                        newDB <- inputProcessing db x
                                        terminalEntrance newDB

terminalTermination :: IO ()
terminalTermination = putStrLn "Quitting Terminal"

inputProcessing :: Database -> String -> IO Database
inputProcessing db x    | compareWord "read " x = do  
    readSQLFromFile (dropWord "read " x) >>= print
    return db
                        | compareWord "open " x = do
    readDBFromFile (dropWord "open " x) >>= print
    return db  
                        | compareWord "save " x = do
    saveToFile (dropWord "save " x) >>= print
    return db                    
                        | compareWord "SELECT" x = do
    print (choiceParser parseTableSelection x)
    return db                    
                        | otherwise = do
    putStrLn x
    return db


compareWord :: String -> String -> Bool
compareWord t input = t == take l input
    where
        l = length t

dropWord :: String -> String -> String
dropWord d = drop l
    where
        l = length d
