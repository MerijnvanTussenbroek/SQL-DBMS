module Folders.TableDeletionFolder where

import AST
import Algebra
import Folder
import Library.Database

creationFolder :: Database -> TableCreation -> Database
creationFolder db creation = undefined

creationAlgebra :: SQLAlgebra expr fkc1 cc cd tc tcr ti td ts st p env
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


cprogram = undefined

ctableCreation = undefined
ctableInsertion = undefined
ctableDeletion = undefined
ctableSelection = undefined

ccreate = undefined
cinsertInto = undefined
cdelete = undefined
cselect = undefined

ctabPrimKey = undefined
ctabCheck = undefined
ctabForeKey = undefined

ccolDef = undefined
cforeignKeyClause1 = undefined

cbinaryExpression = undefined
cliteral = undefined

