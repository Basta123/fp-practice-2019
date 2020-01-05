module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Operators = Plus | Subtraction | Multiply deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, op :: Operators, rhv :: Term } -- бинарная операция
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
replaceVar varName replacement expression = todo

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = todo