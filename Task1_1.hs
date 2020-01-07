module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Operator = Plus | Subtraction | Multiply deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, op :: Operator, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l Plus r 
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l Subtraction r 
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l Multiply r
infixl 7 |*|



-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = 
        case expression of
            Variable var 
              | var == varName -> replacement
              | otherwise -> expression
            BinaryTerm l op r -> BinaryTerm (replaceVar varName replacement l) op (replaceVar varName replacement r)  
            _ -> expression




-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
    BinaryTerm l op r ->
        case (left, op, right) of
            (            _, Plus,    IntConstant 0)  -> left
            (IntConstant 0, Plus,                _)  -> right
            (IntConstant one, Plus,  IntConstant two)  -> IntConstant (one+two)
            (            _, Subtraction,   IntConstant 0)  -> left
            (IntConstant one, Subtraction, IntConstant two)  -> IntConstant (one-two)
            (            _, Multiply,    IntConstant 0)  -> IntConstant 0
            (IntConstant 0, Multiply,                _)  -> IntConstant 0
            (            _, Multiply,    IntConstant 1)  -> left
            (IntConstant 1, Multiply,                _)  -> right
            (IntConstant one, Multiply,  IntConstant two)  -> IntConstant (one*two)
            _                                        -> BinaryTerm left op right
        where
            left = evaluate l
            right = evaluate r
    _                 -> expression 