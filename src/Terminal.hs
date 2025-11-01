module Terminal where

terminalEntrance :: IO ()
terminalEntrance = do
                input <- getLine
                terminalMaintenance input

terminalMaintenance :: String -> IO ()
terminalMaintenance x   | x == "exit" = terminalTermination
                        | otherwise = do
                                        inputProcessing x
                                        terminalEntrance

terminalTermination :: IO ()
terminalTermination = putStrLn "Quitting Terminal"

inputProcessing :: String -> IO ()
inputProcessing x = putStrLn x
