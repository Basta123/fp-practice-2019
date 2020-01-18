module Task5_2 where

import Todo(todo)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst


instance (Show a) => Show (Zipper a) where
   show (Zipper left right) = show (reverse left) ++ show right

instance (Eq a) => Eq (Zipper a) where
   zip1 == zip2 = equality zip1 zip2 
   zip1 /= zip2 = not (zip1 == zip2)


equality :: (Eq a) => Zipper a -> Zipper a -> Bool 
equality (Zipper l1 r1) (Zipper l2 r2) = (l1 ++ r1) == (l2 ++ r2)    


goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка


concatZip :: Zipper a -> Zipper a -> Zipper a
concatZip left right = concat' left right

concat' :: Zipper a -> Zipper a -> Zipper a
concat' (Zipper left right) (Zipper left2 right2) = Zipper (left ++ right) (left2 ++ right2)

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index (Zipper [] []) to   = to
insertManyAt index from (Zipper [] []) = from
insertManyAt index from (Zipper leftI rightI) = if  index < 0 || index > (length (leftI++rightI)) then error "The index is wrong" 
                                         else insertManyAt' index from (Zipper [] (leftI++rightI))


  where 
   insertManyAt' :: Int -> Zipper a -> Zipper a -> Zipper a
   insertManyAt' 0 (Zipper left right) (Zipper left2 right2) = Zipper (left2++left) (right++right2)
   insertManyAt' index from into                             = insertManyAt' (index - 1) from $ goRight into


subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to zipper = case zipper of 
                             (Zipper [] []) -> error "Zipper is empty"
                             (Zipper leftI rightI) | from < 0 || from > length (leftI ++ rightI) -> error "Incorrect"
                                                         | to < 0 || to > length (leftI ++ rightI) -> error "Incorrect"
                                                         | from > to -> error "Incorrect"
                                                         | otherwise -> Zipper [] $take (to - from + 1) $drop from $ leftI ++ rightI