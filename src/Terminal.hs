module Terminal where

import IO
import Parser
import Library.Database

import Data.Maybe

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
    parsed <- readSQLFromFile (dropWord "read " x)
    if isNothing parsed 
        then putStrLn "Couldn't parse file."
        else putStrLn "Applying statements in the file"
    return (if isNothing parsed 
                then db 
                else runStatements (fromJust parsed) db)


                        | compareWord "open " x = do
    newDB <- readDBFromFile (dropWord "open " x)
    if isNothing newDB 
        then putStrLn "Couldn't parse file." 
        else putStrLn "Opening new database" 
    return (fromMaybe db newDB)


                        | compareWord "save " x = do
    saveToFile db (dropWord "save " x) >>= print
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
