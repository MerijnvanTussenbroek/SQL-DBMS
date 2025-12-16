module Evaluator where

import AST
import Library.Database



constructExpression :: [(Name,Values)] -> Expression -> Expression
constructExpression list (BinaryExpression op expr1 expr2) = BinaryExpression op nexpr1 nexpr2
    where
        nexpr1 = constructExpression list expr1
        nexpr2 = constructExpression list expr2

constructExpression list (Literal (Variable s)) = Literal (findValue list s)
constructExpression _ e = e

findValue :: [(Name, Values)] -> Name -> Values
findValue ((name, v):xs) n  | name == n = v
                            | otherwise = findValue xs n
findValue [] _ = NULL

evaluate :: Expression -> Values
evaluate (Literal v) = v
evaluate (BinaryExpression op expr1 expr2)  | nexpr1 == NULL || nexpr2 == NULL = NULL
                                             
                                            | otherwise = calculate op nexpr1 nexpr2

    where
        nexpr1 = evaluate expr1
        nexpr2 = evaluate expr2



calculate :: Operator -> Values -> Values -> Values
calculate Mul (Integer i1) (Integer i2) = Integer (i1 * i2)
calculate Div (Integer i1) (Integer i2) = Integer (i1 `div` i2)
calculate Mod (Integer i1) (Integer i2) = Integer (i1 `mod` i2)
calculate Add (Integer i1) (Integer i2) = Integer (i1 + i2)
calculate Min (Integer i1) (Integer i2) = Integer (i1 - i2)

calculate LessThan (Integer i1) (Integer i2) = Boolean (i1 < i2)
calculate GreaterThan (Integer i1) (Integer i2) = Boolean (i1 > i2)
calculate EqualComp (Integer i1) (Integer i2) = Boolean (i1 == i2)
calculate GreaterThanOrEqual (Integer i1) (Integer i2) = Boolean (i1 == i2 || i1 > i2)
calculate LessThanOrEqual (Integer i1) (Integer i2) = Boolean (i1 == i2 || i1 < i2)

calculate EqualComp (Boolean b1) (Boolean b2) = Boolean (b1 == b2)
calculate LogicalAnd (Boolean b1) (Boolean b2) = Boolean (b1 && b2)
calculate LogicalOr (Boolean b1) (Boolean b2) = Boolean (b1 || b2)

calculate EqualComp (Text s1) (Text s2) = Boolean (s1 == s2)
calculate EqualComp (Character c1) (Character c2) = Boolean (c1 == c2)
calculate EqualComp (VarChar v1) (VarChar v2) = Boolean (v1 == v2)

calculate NotEqualComp v1 v2 = Boolean (not b)
    where
        (Boolean b) = calculate EqualComp v1 v2

calculate _ _ _ = NULL


myZipper :: [Name] -> Row -> [(Name, Values)]
myZipper names (Row v) = zip names v

unZipa :: [(a,b)] -> [a]
unZipa ((a,_):xs) = a : unZipa xs
unZipa [] = []

retrieveVals :: [(Values, a)] -> [(Bool, a)]
retrieveVals ((Boolean b, a):xs) = (b,a) : retrieveVals xs
retrieveVals ((_,a):xs) = (False, a) : retrieveVals xs
retrieveVals [] = []

sortThem :: ([a],[a]) -> [(Bool, a)] -> ([a],[a])
sortThem (l1,l2) ((True, a):xs) = sortThem (a:l1, l2) xs
sortThem (l1,l2) ((False, a):xs) = sortThem (l1, a:l2) xs
sortThem alg [] = alg
