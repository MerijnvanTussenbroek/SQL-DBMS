module Folders.TableSelectionFolder where

import AST
import Algebra
import Folder
import Library.Database

selectionFolder :: Table -> TableSelection -> (Table, String)
selectionFolder db selection = newTable
    where
        newTable = sqlFolder selectionAlgebra db (Program [TableSelection selection])

selectionAlgebra :: SQLAlgebra expr fkc1 cc cd tc tcr ti td ts st p env
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


sprogram = undefined

stableCreation = undefined
stableInsertion = undefined
stableDeletion = undefined
stableSelection = undefined

screate = undefined
sinsertInto = undefined
sdelete = undefined
sselect = undefined

stabPrimKey = undefined
stabCheck = undefined
stabForeKey = undefined

scolDef = undefined
sforeignKeyClause1 = undefined

sbinaryExpression = undefined
sliteral = undefined

