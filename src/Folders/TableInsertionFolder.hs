module Folders.TableInsertionFolder where

import AST
import Algebra
import Folder
import Evaluator
import Library.Database

insertInto :: Database -> TableCreation -> Database
insertInto = undefined

insertionFolder :: Table -> TableInsertion -> (Table, String)
insertionFolder db creation = newTable
    where
        newTable = sqlFolder insertionAlgebra db (Program [TableInsertion creation])

insertionAlgebra :: SQLAlgebra Values fkc1 cc cd tc tcr String td ts String String Table
insertionAlgebra = SQLAlgebra
    iprogram

    undefined
    itableInsertion
    itableDeletion
    itableSelection

    icreate
    iinsertInto
    idelete
    iselect

    itabPrimKey
    itabCheck
    itabForeKey

    icolDef
    iforeignKeyClause1
    ibinaryExpression
    iliteral

iprogram :: env -> [[s]] -> (env, [s])
iprogram env stats = (env, concat stats)


itableInsertion :: a -> b -> (a,b)
itableInsertion env ti = (env, ti)

itableDeletion = undefined
itableSelection = undefined

icreate = undefined

iinsertInto :: Table -> Name -> [Name] -> [Values] -> (Table, String)
iinsertInto alg@(Table n vals cons rows) name names values      | check && n == name = (Table n vals cons (Row values:rows), "Succesfully inserted the row.")
                                                            | otherwise = (alg, "Failed to insert due to wrong variable matching.")
    where
        check = checker vals names (map toVartype values)
iinsertInto EmptyTable _ _ _ = (EmptyTable, "How did we get here? Inserting into an empty table? Crazy.")

idelete = undefined
iselect = undefined

itabPrimKey = undefined
itabCheck = undefined
itabForeKey = undefined

icolDef = undefined
iforeignKeyClause1 = undefined

ibinaryExpression :: t -> Operator -> Values -> Values -> (t, Values)
ibinaryExpression env op e1 e2 = (env, calculate op e1 e2)
iliteral :: Table -> Values -> (Table, Values)
iliteral env v = (env, v)

checker :: Eq a => Eq b => [(a,b)] -> [a] -> [b] -> Bool
checker (x:xs) (y:ys) (z:zs)    | checkFunc x y z = checker xs ys zs
                                | otherwise = False
checker [] [] [] = True
checker _ _ _ = False

checkFunc :: Eq a => Eq b => (a,b) -> a -> b -> Bool
checkFunc (a,b) x y = a == x && b == y

toVartype :: Values -> VarType
toVartype (Integer _) = INT
toVartype (Text _) = TEXT
toVartype (Boolean _) = BOOL
toVartype (Character c) = CHAR 1
toVartype (VarChar c) = VARCHAR (length c)
toVartype _ = Null
