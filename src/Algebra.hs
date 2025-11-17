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

    binaryExpression :: env -> Operator -> expr -> expr -> (env, expr),
    literal :: env -> v -> (env, expr),


    integer :: env -> Int -> (env, v),
    text :: env -> String -> (env ,v),
    boolean :: env -> Bool -> (env, v),
    varChar :: env -> String -> (env, v),
    character :: env -> Char -> (env, v),
    variable :: env -> String -> (env, v)

    }

runOver :: env -> [s] -> [o] -> (env -> s -> (env, o)) -> (env, [o])
runOver env (x:xs) total f = let (tempEnv, tempO) = f env x in runOver tempEnv xs (total ++ [tempO]) f
runOver env [] total _ = (env, total)


sqlFolder :: SQLAlgebra v expr fkc1 cc cd tc tcr ti td ts st p env -> env -> Program -> (env, p)
sqlFolder SQLAlgebra {..} = pfold
    where


        pfold env (Program stats) = program env2 total
            where
                (env2, total) = runOver env stats [] stfold


        stfold env (TableCreation _) = undefined

        stfold env (TableInsertion _) = undefined

        stfold env (TableDeletion _) = undefined

        stfold env (TableSelection _) = undefined

        cfold env name cds tcs = undefined

        iifold env name names exprs = undefined

        dfold env name exprs = undefined

        sfold env (Just b) names1 names2 expr = undefined

        sfold env Nothing names1 names2 expr = undefined

        tpkfold env (Just n) name = undefined
        tpkfold env Nothing name = undefined

        tcfold env (Just n) expr = undefined
        tcfold env Nothing expr = undefined

        tfkfold env (Just n) foreignKey = undefined
        tfkfold env Nothing foreignKey = undefined

        cdfold env name vartype (Just cc) = undefined
        cdfold env name vartype Nothing = undefined

        fkc1fold env name1 name2 name3 = undefined

        befold env op expr1 expr2 = undefined
        lfold env v = undefined

        intfold env i = undefined
        textfold env st = undefined
        boolfold env b = undefined
        vchfold env s = undefined
        chfold env c = undefined
        varfold env st = undefined


            



