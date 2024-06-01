module Ej1 where

data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

(|||):: a -> b -> (a,b)
(|||) a b = (a,b)

nth:: BTree a -> Int -> a
nth (Node _ Empty x _) 0 = x
nth (Node _ Empty _ r) n = nth r $ n-1
nth (Node _ l@(Node sl _ _ _) x r) n
                                | n < sl = nth l n
                                | n == sl = x
                                | otherwise = nth r (n-sl-1)

cons:: a -> BTree a -> BTree a
cons a Empty = Node 1 Empty a Empty
cons a (Node s l x r) = Node (s+1) (cons a l) x r

size:: BTree a -> Int
size Empty = 0
size (Node s _ _ _) = s

{-
    W(n) = 2*W(n/2)+c -> O(n)
    S(n) = S(n/2)+c -> O(log n)
-}
tabulate :: (Int -> a) -> Int -> BTree a
tabulate f = tabulate' f 0
    where
        tabulate':: (Int -> a) -> Int -> Int -> BTree a
        tabulate' f i j
                | j <= i = Empty
                | j-1==i = Node 1 Empty (f i) Empty
                | otherwise = let
                                m = div (i+j) 2
                                (l,r) = tabulate' f i m ||| tabulate' f (m+1) j
                              in
                                    Node (size l + 1 + size r) l (f m) r
{-
    W(st) = W(sl) + W(sr) + c
    S(st) = max(S(sl),S(sr)) + c
-}
mapT:: (a->b) -> BTree a -> BTree b
mapT _ Empty = Empty
mapT f (Node s l x r) = let
                            (ml,mr) = mapT f l ||| mapT f r
                        in
                            Node s ml (f x) mr

takeT :: Int -> BTree a -> BTree a
takeT _ Empty = Empty
takeT 0 _ = Empty
takeT n (Node s l x r)
                    | n <= size l = takeT n l
                    | otherwise = let 
                                    tr = takeT (n - size l - 1) r
                                  in
                                    Node (size l + 1 + size tr) l x tr 

dropT :: Int -> BTree a -> BTree a
dropT _ Empty = Empty
dropT 0 t = t
dropT n (Node s l x r)
                    | n <= size l = let
                                        dl = dropT n l
                                    in
                                        Node (size dl + 1 + size r) dl x r
                    | otherwise = dropT (n - size l - 1) r