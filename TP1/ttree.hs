module Ttree where

import Data.Maybe (isNothing, isJust, fromJust)

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | Leaf k v | E deriving Show

-- Algunas funciones para acceder a los valores del ttree
key:: TTree k v -> k
key (Leaf k _) = k
key (Node k _ _ _ _) = k

value:: TTree k v -> Maybe v
value E = Nothing
value (Leaf _ v) = Just v
value (Node _ v _ _ _) = v

left:: TTree k v -> TTree k v
left (Node _ _ l _ _ ) = l
left _ = E

mid:: TTree k v -> TTree k v
mid (Node _ _ _ m _ ) = m
mid _ = E

right:: TTree k v -> TTree k v
right (Node _ _ _ _ r ) = r
right _ = E

-- Esta funcion verifica si el arbol es vacio
isE :: TTree k v -> Bool
isE E = True
isE _ = False


-- Busca una clave en el arbol
search:: Ord k => [k] -> TTree k v -> Maybe v
search [] _ = Nothing
search _ E = Nothing
search s@(x:xs) t
        | x < key t = search s $ left t
        | x > key t = search s $ right t
        | null xs = value t
        | otherwise = search xs $ mid t

-- Cambia el valor de la raiz del arbol
setValue:: v -> TTree k v -> TTree k v
setValue _ E = E
setValue v (Leaf k _) = Leaf k v
setValue v (Node k _ l m r) = Node k (Just v) l m r

-- Inserta una clave en el arbol
insert:: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [] _ t = t

insert [x] v E = Leaf x v
insert (x:xs) v E = Node x Nothing E (insert xs v E) E

insert s@(x:xs) nuevoValor t
                | x < k = Node k v' (insert s nuevoValor l) m r
                | x > k = Node k v' l m (insert s nuevoValor r) 
                | null xs = setValue nuevoValor t
                | otherwise = Node k v' l (insert xs nuevoValor m) r
                where
                        k = key t
                        v' = value t
                        l = left t
                        m = mid t
                        r = right t


{-
Funcion auxiliar para delete. Recibe un nodo I y segun el caso se ve modificado,
eliminado o permanece sin cambios.
I es eliminado si:
1- No tiene hijos
2- no almacena valor
caso contrario no se modifica nada. Si (1) pero no (2), se convierte a hoja. Si (2)
pero no (1), ocurre uno de los siguientes casos:
- si no tiene hijo medio pero si izquierdo, se elimina I y en su lugar pasa a estar su
hijo izquierdo (si I tenia hijo derecho, este se coloca como hijo derecho del descendiente
mas a la derecha de su hijo izquierdo)
- si no tiene hijo medio ni izquierdo, tendra hijo derecho. En tal caso, analago a lo anterior
pero con este ultimo.
- si no se cumple ningun caso anterior, entonces tendra hijo medio por lo que no se elimina.
-}
modifyNode :: TTree k v -> TTree k v
modifyNode E = E
modifyNode (Node k v l m r) | eM && eL && eR = if existValue then Leaf k (fromJust v) else E
                            | existValue = Node k v l m r
                            | eM && not eL = if not eR
                                            then Node (key l) (value l) (left l) (mid l) (aux r (right l))
                                            else Node (key l) (value l) (left l) (mid l) r
                            | eM && not eR = Node (key r) (value r) l (mid r) (right r)
                            | otherwise = Node k v l m r where
                                eM = isE m
                                eL = isE l
                                eR = isE r
                                existValue = isJust v
                                {- dados A y B, aux coloca a A como hijo derecho del descendiente mas
                                 a la derecha de B. -}
                                aux t E = t
                                aux t (Leaf k v) = Node k (Just v) E E t
                                aux t (Node k v l m r) = Node k v l m (aux t r)


{-
Elimina la clave y su valor asociado del arbol. A medida que se buscan los nodos de la clave
a eliminar, se llama modifyNode sobre estos. Una vez que terminan los llamados recursivos
de delete, se resuelven los llamados de modifyNode de abajo hacia arriba.
-}
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete _ E = E
delete [] t = t
delete (x:xs) t | x < k = modifyNode (Node k v (delete (x:xs) l) m r)
                | x > k = modifyNode (Node k v l m (delete (x:xs) r))
                | x == k = if null xs
                                    then modifyNode (deleteValue t)
                                    else modifyNode (Node k v l (delete xs m) r) where
                    k = key t
                    v = value t
                    l = left t
                    m = mid t
                    r = right t
                    deleteValue (Node k v l m r) = Node k Nothing l m r
                    deleteValue (Leaf k v) = E

-- retorna las claves de menor a mayor
keys :: TTree k v -> [[k]]
-- Prefijo empieza en [], los prefijos se van a agregar a una lista vacia
keys t = keys' [] t []
        where
                -- Devuelve una composicion de funciones que agregan las llaves de manera ordenada a una lista dada
                -- Ejemplo: t= insert "reo" _ $ insert "res" 21 E
                -- keys' id . id . ("reo":) . ("res":) . id . id . id . id
                -- prefix es el prefijo que se va a agregar, se construye a medida que se baja por el arbol
                -- Los reverse solo se ejecutan cuando se llega al fin de un prefijo
                keys':: [k] -> TTree k v -> ([[k]] -> [[k]])
                keys' _ E = id
                keys' prefix (Leaf k _) = (reverse (k:prefix) :)
                
                -- Cuando keys' llega a E devuelve la funcion id, esto puede generar que se componga muchas veces id
                -- Podria optimizarse ignorando las llamadas a keys' E con pattern matching para asi evitar la composicion
                -- repetida de funciones id, pero eso haria el codigo menos legible asi que lo deje asi.
                keys' prefix (Node k Nothing l m r) = keys' prefix l . keys' (k:prefix) m . keys' prefix r
                keys' prefix (Node k (Just _) l m r) = keys' prefix l . (reverse (k:prefix) :) . keys' (k:prefix) m . keys' prefix r 

-- El arbol que muestra el enunciado
-- t :: TTree Char Integer
-- t = insert "se" 8 $ insert "si" 4 $ insert "sin" 7 $ insert "ras" 1 $ insert "re" 16 $ insert "red" 9 $ insert "res" 4 $ insert "reo" 2 E
