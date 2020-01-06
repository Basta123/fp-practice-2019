module Task1_2 where

import Prelude hiding (gcd)

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел		
gcd :: Integer -> Integer -> Integer
gcd x 0 = x
gcd 0 x = x
gcd n m = let a = abs n
              b = abs m
		  in gcd (b `mod` a) a	


-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = (ceiling (sqrt (fromIntegral from))) <= (floor (sqrt (fromIntegral to)))

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y  
    | y == 0 && x == 0 = error "not correct"
    | y == 0 = 1
    | x == 0 = 0
    | y == 1 = x
    | otherwise = x * pow x (y-1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = if x > 1 then null [ y | y <- [2..x - 1], x `mod` y == 0] else False

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo