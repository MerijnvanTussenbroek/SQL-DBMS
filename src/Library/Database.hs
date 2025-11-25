module Library.Database where

import AST

data Database = DB Name [Table]
                | EmptyDB
                | ReturnColumns [Row]

data Table =    Table Name [(Name, VarType)] [TableConstraint] [Row]
                | EmptyTable
                deriving Show

data Row = Row [Values]
        deriving Show

