module Library.Database where

import AST

data Database = DB !Name ![Table]
                | EmptyDB

data Table =    Table !Name ![(Name, VarType)] ![TableConstraint] ![Row]
                | EmptyTable
                | ReturnColumns [Row]
                deriving Show

changeTable :: [Table] -> Table -> [Table]
changeTable (x@(Table oname _ _ _):xs) alg@(Table nname _ _ _)    | oname == nname = alg : xs
                                                                | otherwise = x : changeTable xs alg
changeTable [] _ = []
changeTable list _ = list

data Row = Row ![Values]
        deriving Show

retrieveTable :: Name -> Database -> Maybe Table
retrieveTable name EmptyDB = Nothing
retrieveTable tableName (DB p list) = f tableName list
    where
        f name (alg@(Table n _ _ _):xs) = if name == n then Just alg else f name xs 
        f name (_:xs) = f name xs
        f name [] = Nothing


addTable :: Database -> Table -> Database
addTable EmptyDB table = DB "Database" [table]
addTable (DB name tables) table = DB name (table:tables)
