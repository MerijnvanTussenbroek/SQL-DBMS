module Library.Database where

import AST

data Database = DB Name [Table]
                | EmptyDB

data Table =    Table Name [(Name, VarType)] [TableConstraint] [Row]
                | EmptyTable
                | ReturnColumns [Row]
                deriving Show

data Row = Row [Values]
        deriving Show

