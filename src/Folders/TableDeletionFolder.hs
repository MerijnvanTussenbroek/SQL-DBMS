module Folders.TableDeletionFolder where

import AST
import Algebra
import Folder
import Library.Database
import Evaluator

delete :: Database -> TableDeletion -> (Database, String)
delete db@(DB dbName tables) stat@(Delete name expr) = (DB dbName (changeTable tables newTable), string)
    where
        (newTable, string) = maybe (EmptyTable, "Unable to delete") (deletionFolder stat) (retrieveTable name db)

deletionFolder :: TableDeletion -> Table -> (Table, String)
deletionFolder deletion db = newTable
    where
        newTable = sqlFolder deletionAlgebra db (Program [TableDeletion deletion])
        

deletionAlgebra :: SQLAlgebra Expression fkc1 cc cd tc tcr ti String ts String String Table
deletionAlgebra = SQLAlgebra
    dprogram

    dtableCreation
    dtableInsertion
    dtableDeletion
    dtableSelection

    dcreate
    dinsertInto
    ddelete
    dselect

    dtabPrimKey
    dtabCheck
    dtabForeKey

    dcolDef
    dforeignKeyClause1
    dbinaryExpression
    dliteral


dprogram :: env -> [[s]] -> (env, [s])
dprogram env strings = (env, concat strings)

dtableCreation = undefined
dtableInsertion = undefined
dtableDeletion :: env -> [s] -> (env, [s])
dtableDeletion env ts = (env, ts)
dtableSelection = undefined

dcreate = undefined
dinsertInto = undefined

ddelete :: Table -> Name -> Expression -> (Table, String)
ddelete (Table tname vals cons rows) name exps = (Table tname vals cons l2, "Deleted:" ++ show l1)
    where
        names = unZipa vals
        zipped = map (myZipper names) rows
        expressions = map (`constructExpression` exps) zipped
        evaluated = map evaluate expressions
        keepEmOrNot = zip evaluated rows
        (l1,l2) = sortThem ([],[]) (retrieveVals keepEmOrNot)
        


ddelete EmptyTable _ _ = (EmptyTable, "Something went wrong. Tried deleting from an empty table")

dselect = undefined

dtabPrimKey = undefined
dtabCheck = undefined
dtabForeKey = undefined

dcolDef = undefined
dforeignKeyClause1 = undefined

dbinaryExpression :: env -> Operator -> Expression -> Expression -> (env, Expression)
dbinaryExpression env op e1 e2 = (env, BinaryExpression op e1 e2)

dliteral :: env -> Values -> (env, Expression)
dliteral env v = (env, Literal v)


