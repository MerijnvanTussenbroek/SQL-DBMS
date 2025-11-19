{-# LANGUAGE  RecordWildCards #-}

module Folder where

import AST
import Algebra


runOver :: env -> [s] -> [o] -> (env -> s -> (env, o)) -> (env, [o])
runOver env (x:xs) total f = let (tempEnv, tempO) = f env x in runOver tempEnv xs (total ++ [tempO]) f
runOver env [] total _ = (env, total)


sqlFolder :: SQLAlgebra expr fkc1 cc cd tc tcr ti td ts st p env -> env -> Program -> (env, p)
sqlFolder SQLAlgebra {..} = pfold
    where
        pfold env (Program stats) = program env2 total
            where
                (env2, total) = runOver env stats [] stfold


        stfold env (TableCreation creation) = tableCreation env2 c
            where
                (env2, c) = cfold env creation

        stfold env (TableInsertion insertion) = tableInsertion env2 i
            where
                (env2, i) = iifold env insertion

        stfold env (TableDeletion deletion) = tableDeletion env2 d
            where
                (env2, d) = dfold env deletion

        stfold env (TableSelection selection) = tableSelection env2 s
            where
                (env2, s) = sfold env selection

        cfold env (Create name defs cons) = create env3 name d c
            where
                (env2, d) = runOver env defs [] cdfold
                (env3, c) = runOver env2 cons [] tcofold

        iifold env (InsertInto name names expr) = insertInto env2 name names e
            where
                (env2, e) = runOver env expr [] exprfold

        dfold env (Delete name expr) = delete env2 name e
            where
                (env2, e) = runOver env expr [] exprfold

        sfold env (Select b names1 names2 expr) = select env2 b names1 names2 e
            where
                (env2, e) = exprfold env expr

        tcofold env (TabPrimKey n name) = tabPrimKey env n name

        tcofold env (TabCheck n expr) = tabCheck env2 n e
            where
                (env2, e) = exprfold env expr

        tcofold env (TabForeKey n foreignKey) = tabForeKey env2 n fk
            where
                (env2, fk) = fkc1fold env foreignKey

        cdfold env (ColDef name vartype cc) = colDef env name vartype Nothing

        fkc1fold env (ForeignKeyClause1 name1 name2 name3) = foreignKeyClause1 env name1 name2 name3

        exprfold env (BinaryExpression op expr1 expr2) = binaryExpression env3 op e1 e2
            where
                (env2, e1) = exprfold env expr1
                (env3, e2) = exprfold env2 expr2

        exprfold env (Literal value) = literal env value


