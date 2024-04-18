data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | Leaf k v | E deriving Show

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

search:: Ord k => [k] -> TTree k v -> Maybe v
search [] _ = Nothing
search _ E = Nothing
search (x:xs) t
        | x < key t = search (x:xs) (left t)
        | x > key t = search (x:xs) (right t)
        | otherwise = if null xs then value t else search xs (mid t) 

setValue:: v -> TTree k v -> TTree k v
setValue _ E = E
setValue v (Leaf k _) = Leaf k v
setValue v (Node k _ l m r) = Node k (Just v) l m r

insert:: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [] _ t = t

insert [x] v E = Leaf x v
insert (x:xs) v E = Node x Nothing E (insert xs v E) E

insert (x:xs) v t
                | x < k = Node k v' (insert (x:xs) v l) m r
                | x > k = Node k v' l m (insert (x:xs) v r) 
                | otherwise = if null xs then setValue v t else Node k v' l (insert xs v m) r
                where
                        k = key t
                        v' = value t
                        l = left t
                        m = mid t
                        r = right t