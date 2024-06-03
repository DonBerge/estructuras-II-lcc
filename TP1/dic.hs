{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
import Ttree

class Dic k v d | d -> k v where
    vacio :: d
    insertar :: Ord k => k -> v -> d -> d
    buscar :: Ord k => k -> d -> Maybe v
    eliminar :: Ord k => k -> d -> d
    claves :: Ord k => d -> [k ]

instance Ord k => (Dic [k] v) (TTree k v) where
    vacio :: Ord k => TTree k v
    vacio = E

    insertar :: (Ord k) => [k] -> v -> TTree k v -> TTree k v
    insertar = insert
    
    buscar :: (Ord k) => [k] -> TTree k v -> Maybe v
    buscar = search
    
    eliminar :: (Ord k) => [k] -> TTree k v -> TTree k v
    eliminar = delete
    
    claves :: (Ord k) => TTree k v -> [[k]]
    claves = keys