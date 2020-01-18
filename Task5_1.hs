module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов


index :: DList a -> Int -> a
index  DNil _ = error "Empty"
index (DCons _ c _) 0 = c
index (DCons l _ r) i = if i < 0 then index l (i + 1) else index r (i - 1)


insertAt :: DList a -> Int -> a -> DList a
insertAt  DNil         index value = if index == 0 then DCons DNil value DNil else error "incorrect index"
insertAt (DCons left curr right) index value = if index == 0 then x else DCons left curr $ insertAt right (index-1) value
                                             where
                                                x = DCons left value y
                                                y = DCons x curr right


removeAt :: DList a -> Int -> DList a
removeAt DNil _                       = error "Empty"
removeAt (DCons _ _ DNil) index = if index == 0 then DNil else error "Invalid index"
removeAt (DCons l _ (DCons _ rc rt)) 0 = DCons l rc rt
removeAt (DCons l value r) index   = DCons l value $ removeAt r (index - 1)
