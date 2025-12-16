module Terminal where

import IO
import Parser
import Library.Database

import Data.Maybe

import AST

import Folders.TableSelectionFolder (select)
import Folders.TableInsertionFolder (insertInto)
import Folders.TableDeletionFolder (delete)
import Folders.TableCreationFolder (create)

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
    let (newDB, stats) = maybe (db, ["Couldn't parse file"]) (applyFolders [] db) parsed
    putStrLn (unlines stats)
    return newDB 


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
    let selectStat = choiceParser parseTableSelection x
    let (st, ss) = maybe (EmptyTable, "Couldn't parse statement") (select db) selectStat
    putStrLn ss
    print st
    return db 
                        | compareWord "INSERT INTO" x = do
    let insertStat = choiceParser parseTableInsert x
    let (it, is) = maybe (db, "Couldn't parse statement") (insertInto db) insertStat
    putStrLn is
    return it
                        | compareWord "DELETE" x = do
    let deleteStat = choiceParser parseTableDelete x
    let (dt, ds) = maybe (db, "Couldn't parse statement") (delete db) deleteStat
    putStrLn ds
    return dt
                        | compareWord "CREATE" x = do
    let createStat = choiceParser parseCreateStatement x
    let (ct, cs) = maybe (db, "Couldn't parse statement") (create db) createStat
    putStrLn cs
    return ct
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

applyFolders :: [String] -> Database -> [Statements] -> (Database, [String])
applyFolders l db ((TableCreation stat):xs) = applyFolders (cs:l) ct xs
    where
        (ct, cs) = create db stat 
applyFolders l db ((TableInsertion stat):xs) = applyFolders (is:l) it xs
    where
        (it, is) = insertInto db stat
applyFolders l db ((TableDeletion stat):xs) = applyFolders (ds:l) dt xs
    where
        (dt, ds) = delete db stat
applyFolders l db ((TableSelection stat):xs) = applyFolders (show st:ss:l) db xs
    where
        (st, ss) = select db stat
applyFolders l db [] = (db, reverse l)



