module Algebra where

import AST


data SQLAlgebra expr fkc1 cc cd tc tcr ti td ts st p env = SQLAlgebra {
    
    program :: env -> [st] -> (env, p),

    tableCreation :: env -> tcr -> (env, st),
    tableInsertion :: env -> ti -> (env, st),
    tableDeletion :: env -> td -> (env, st),
    tableSelection :: env -> ts -> (env, st),

    create :: env -> Name -> [cd] -> [tc] -> (env, tcr),
    insertInto :: env -> Name -> [Name] -> [expr] -> (env, ti),
    delete :: env -> Name -> expr -> (env, td),
    select :: env -> Maybe Bool -> [Name] -> Name -> expr -> (env,ts),

    tabPrimKey :: env -> Maybe Name -> Name -> (env, tc),
    tabCheck :: env -> Maybe Name -> expr -> (env, tc),
    tabForeKey :: env -> Maybe Name -> fkc1 -> (env, tc),

    colDef :: env -> Name -> VarType -> Maybe cc -> (env, cd),

    foreignKeyClause1 :: env -> Name -> Name -> Name -> (env, fkc1),

    binaryExpression :: env -> Operator -> expr -> expr -> (env, expr),
    literal :: env -> Values -> (env, expr)

    }


