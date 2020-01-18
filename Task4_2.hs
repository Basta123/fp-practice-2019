module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where

   -- применяем нашу функцию f к каждому элементу "протащив" функцию через контейнер, после применения значения заворачиваются обратно в контейнер.
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)



instance Applicative FourOf where
   -- делает обертку дублируясь 4 раза 
   pure a = FourOf a a a a
  -- оператор <*> извлекает наши функции из обертки и применяет к аргументам(которые также вне обертки) затем все обратно упаковывается.
   (FourOf f1 f2 f3 f4) <*> (FourOf a1 a2 a3 a4) = FourOf (f1 a1) (f2 a2) (f3 a3) (f4 a4)

instance Monad FourOf where

   return a = FourOf a a a a
   -- достаем значения из обертки и применяем к функции func, который возвращает значения в обертке(тип FourOf), из полученного контекст мы берем
   --соответствующие элементы, оборачиваем и возвращаем новый контекст.
   
   FourOf a b c d >>= func = FourOf a' b' c' d' where
        FourOf a' _ _ _ = func a 
        FourOf _ b' _ _ = func b
        FourOf _ _ c' _ = func c
        FourOf _ _ _ d' = func d
