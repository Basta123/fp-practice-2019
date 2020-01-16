module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)


foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f (f ini x) xs


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` foldr f ini xs


unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helpFoo (f ini) where
  helpFoo (Just (x,ini')) = x: unfoldr f ini'
  helpFoo Nothing         = []
  


-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst



-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t



-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x acc -> f x : acc) [] xs



-- Произведение всех элементов списка
product :: [Integer] -> Integer
product xs = foldr (*) 1 xs



-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes l = foldr f [] l
    where f (Just a) b  = a : b
          f Nothing b   = b



-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal xs = foldr f [] xs
    where f x s = x !! (length xs - length s - 1) : s


-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f xs = foldr (\x -> \acc -> if f x then acc else x : acc) [] xs



-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem elem' xs = foldr (\x s -> if x == elem' then True else s) False xs


-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from
    where f from | from < to = Just (from, from + step)
                 | otherwise = Nothing 



-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append xs1 xs2 = foldr (:) xs1 xs2 



-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups xs num = unfoldr foo xs 
    where foo []   = Nothing
          foo xs = Just (take (fromIntegral num) xs, drop (fromIntegral num) xs)
