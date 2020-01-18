module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}
import Prelude hiding (lookup,Nothing)
import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Nothing | Node {key :: Integer, 
                                 value :: v, 
                                 leftNode :: TreeMap v, 
                                 rightNode :: TreeMap v} deriving (Show,Eq)



-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Nothing

-- Содержится ли заданный ключ в дереве?
   
         
contains :: TreeMap v -> Integer -> Bool
contains Nothing _ = False
contains (Node key _ leftNode rightNode) specifiedKey = let {goRight = contains rightNode specifiedKey; goLeft = contains leftNode specifiedKey} in 
                                                        if key == specifiedKey then True else if key > specifiedKey then goRight else goLeft

  


-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup specifiedKey (Node key value leftNode rightNode) = let goRight = lookup specifiedKey rightNode 
                                                              goLeft = lookup specifiedKey leftNode  in
                                                              if specifiedKey == key then value else if specifiedKey > key then goRight else goLeft
lookup _ _ = error "specified key doesn't exist"




--Вставка пары (ключ, значение) в дерево
insert :: (Integer, v)  -> TreeMap v -> TreeMap v
insert (specifiedKey, value') Nothing = Node specifiedKey value' Nothing Nothing
insert (specifiedKey, value') (Node key value leftNode rightNode) | specifiedKey == key = error "this key already exists"
                                                        | specifiedKey > key = Node key value leftNode (insert (specifiedKey, value') rightNode)
                                                        | specifiedKey < key = Node key value (insert (specifiedKey, value') leftNode) rightNode





-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ Nothing = Nothing
remove specifiedKey (Node key value leftNode rightNode) 
  | specifiedKey < key = Node key value (remove specifiedKey leftNode ) rightNode
  | specifiedKey > key = Node key value leftNode (remove specifiedKey rightNode )
  | specifiedKey == key = case rightNode of
                              Nothing -> leftNode
                              _ -> (Node leftmaxA leftmaxB leftNode r)
                              where
                                  ((leftmaxA, leftmaxB), r) = deleteLeftmax rightNode
                                  deleteLeftmax (Node key value Nothing rightNode) = ((key, value), rightNode)
                                  deleteLeftmax (Node key value leftNode rightNode) = (pair, Node key value l rightNode)
                                    where (pair, l) = deleteLeftmax leftNode




-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ Nothing = error ("Tree is empty!")
nearestLE specifiedKey (Node key value leftNode rightNode ) | key == specifiedKey = (key, value)
    | key > specifiedKey = nearestLE specifiedKey leftNode
    | key < specifiedKey = case rightNode of
        Nothing -> (key, value)
        (Node key value _ _) -> if key == specifiedKey then (key, value) else nearestLE specifiedKey rightNode
  
   
    



-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList = foldr insert Nothing 

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree (Node key value leftNode rightNode) = [(key, value)] ++ listFromTree(leftNode) ++ listFromTree(rightNode)
listFromTree _ = []
 
-- Поиск k-той порядковой статистики дерева
      

kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Nothing = error "empty"
kMean i (Node key value leftNode rightNode ) = if (treeSize leftNode == i) 
                                               then (key, value) 
                                               else if (treeSize leftNode > i) 
                                               then (kMean i leftNode) 
                                               else (kMean (i - (treeSize leftNode) - 1) rightNode)
treeSize :: TreeMap v -> Integer
treeSize Nothing = 0       
treeSize (Node _ _ leftNode rightNode ) =  1 + (treeSize leftNode) + (treeSize rightNode) 
