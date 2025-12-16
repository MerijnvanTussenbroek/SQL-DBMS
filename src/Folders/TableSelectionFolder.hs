module Folders.TableSelectionFolder where

import AST
import Algebra
import Folder
import Library.Database
import Evaluator


select :: Database -> TableSelection -> (Table, String)
select db selection@(Select _ _ name _) = maybe (EmptyTable, "Couldn't find the table within the database") (selectionFolder selection) tableName
    where
        tableName = retrieveTable name db
        

selectionFolder :: TableSelection -> Table -> (Table, String)
selectionFolder selection table = newTable
    where
        newTable = sqlFolder selectionAlgebra table (Program [TableSelection selection])

selectionAlgebra :: SQLAlgebra Expression fkc1 cc cd tc tcr ti td String String String Table
selectionAlgebra = SQLAlgebra
    sprogram

    stableCreation
    stableInsertion
    stableDeletion
    stableSelection

    screate
    sinsertInto
    sdelete
    sselect

    stabPrimKey
    stabCheck
    stabForeKey

    scolDef
    sforeignKeyClause1
    sbinaryExpression
    sliteral


sprogram :: a -> [String] -> (a, String)
sprogram env st = (env, concat st)

stableCreation = undefined
stableInsertion = undefined
stableDeletion = undefined
stableSelection :: env -> [s] -> (env, [s])
stableSelection env ts = (env, ts)

screate = undefined
sinsertInto = undefined
sdelete = undefined
-- TableSelection = Select (Maybe Bool) [Name] Name Expression

sselect :: Table -> Maybe Bool -> [Name] -> Name -> Expression -> (Table, String)
sselect (Table tableName vals cons rows) _ rowNames _ expr = (ReturnColumns l1, "Selected the following")
    where
        names = unZipa vals
        zipped = map (myZipper names) rows
        expressions = map (`constructExpression` expr) zipped
        evaluated = map evaluate expressions
        keepEmOrNot = zip evaluated rows
        (l1,_) = sortThem ([],[]) (retrieveVals keepEmOrNot)



sselect EmptyTable _ _ _ _ = (EmptyTable, "Tried selecting from an empty table. Table possibly did not exist. Error.")
stabPrimKey = undefined
stabCheck = undefined
stabForeKey = undefined

scolDef = undefined
sforeignKeyClause1 = undefined

sbinaryExpression :: Table -> Operator -> Expression -> Expression -> (Table, Expression)
sbinaryExpression t o e1 e2 = (t, BinaryExpression o e1 e2)

sliteral :: Table -> Values -> (Table, Expression)
sliteral t v = (t, Literal v)

