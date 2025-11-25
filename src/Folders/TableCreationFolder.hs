module Folders.TableCreationFolder where

import AST
import Algebra
import Folder
import Library.Database

create :: Database -> TableCreation -> Database
create = undefined

creationFolder :: TableCreation -> (Table, String)
creationFolder creation = newTable
    where
        newTable = sqlFolder creationAlgebra EmptyTable (Program [TableCreation creation])

-- expr fkc1 cc cd tc tcr ti td ts st p env
creationAlgebra :: SQLAlgebra Expression ForeignKeyClause1 () (Name, VarType) TableConstraint String ti td ts String String Table
creationAlgebra = SQLAlgebra
    cprogram

    ctableCreation
    ctableInsertion
    ctableDeletion
    ctableSelection

    ccreate
    cinsertInto
    cdelete
    cselect

    ctabPrimKey
    ctabCheck
    ctabForeKey

    ccolDef
    cforeignKeyClause1
    cbinaryExpression
    cliteral


cprogram :: Table -> [String] -> (Table, String)
cprogram env stats = (env, concat stats)


ctableCreation :: Table -> String -> (Table, String)
ctableCreation env table = (env, table)

-- these functions should never be reached by this folder
ctableInsertion env table = undefined
ctableDeletion env table = undefined
ctableSelection env table = undefined


ccreate :: Table -> Name -> [(Name, VarType)] -> [TableConstraint] -> (Table, String)
ccreate env name cds tcs = (Table name cds tcs [], "Created a table by the name: " ++ name ++ ". It has the following properties: " ++ show cds ++ " " ++ show tcs)

-- these functions should also never be reached by this folder
cinsertInto env name names exprs = undefined
cdelete env names exprs = undefined
cselect env (Just b) names1 names2 expr = undefined
cselect env Nothing names1 names2 expr = undefined


ctabPrimKey :: Table -> Maybe Name -> Name -> (Table, TableConstraint)
ctabPrimKey env (Just n) name = (env, TabPrimKey Nothing name)
ctabPrimKey env Nothing name = (env, TabPrimKey Nothing name)

ctabCheck :: Table -> Maybe Name -> Expression -> (Table, TableConstraint)
ctabCheck env (Just n) expr = (env, TabCheck Nothing expr)
ctabCheck env Nothing expr = (env, TabCheck Nothing expr)


ctabForeKey :: Table -> Maybe Name -> ForeignKeyClause1 -> (Table, TableConstraint)
ctabForeKey env (Just n) fk = (env, TabForeKey Nothing fk)
ctabForeKey env Nothing fk = (env, TabForeKey Nothing fk)


ccolDef :: Table -> Name -> VarType -> Maybe cc -> (Table, (Name, VarType))
ccolDef env name vt _ = (env, (name, vt))

cforeignKeyClause1 :: Table -> Name -> Name -> Name -> (Table, ForeignKeyClause1)
cforeignKeyClause1 env name1 name2 name3 = (env, ForeignKeyClause1 name1 name2 name3)

cbinaryExpression :: Table -> Operator -> Expression -> Expression -> (Table, Expression)
cbinaryExpression env op e1 e2 = (env, BinaryExpression op e1 e2)
cliteral :: Table -> Values -> (Table, Expression)
cliteral env v = (env, Literal v)

