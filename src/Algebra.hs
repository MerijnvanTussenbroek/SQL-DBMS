{-# LANGUAGE RecordWildCards #-}
module Algebra where

import Library.Environment
import AST


data SQLAlgebra v expr fkc1 cc cd tc tcr ti td ts st p env = SQLAlgebra {
    
    program :: env -> [st] -> (env, p),

    tabeleCreation :: env -> tcr -> (env, st),
    tableInsertion :: env -> ti -> (env, st),
    tableDeletion :: env -> td -> (env, st),
    tableSelection :: env -> ts -> (env, st),

    create :: env -> Name -> [cd] -> [tc] -> (env, tcr),
    insertInto :: env -> Name -> [Name] -> [expr] -> (env, ti),
    delete :: env -> Name -> [expr] -> (env, td),
    select :: env -> Maybe Bool -> [Name] -> [Name] -> expr -> (env,ts),

    tabPrimKey :: env -> Maybe Name -> Name -> (env, tc),
    tabCheck :: env -> Maybe Name -> expr -> (env, tc),
    tabForeKey :: env -> Maybe Name -> fkc1 -> (env, tc),

    colDef :: env -> Name -> VarType -> Maybe cc -> (env, cd),

    foreignKeyClause1 :: env -> Name -> Name -> Name -> (env, fkc1),

    binaryExpression :: Operator -> expr -> expr -> (env, expr),
    literal :: env -> v -> (env, expr),


    integer :: env -> Int -> (env, v),
    text :: env -> String -> (env ,v),
    boolean :: env -> Bool -> (env, v),
    varChar :: env -> String -> (env, v),
    character :: env -> Char -> (env, v),
    variable :: env -> String -> (env, v)

    }

sqlFolder :: SQLAlgebra v expr fkc1 cc cd tc tcr ti td ts st p env -> env -> Program -> (env, p)
sqlFolder SQLAlgebra {..} = undefined



