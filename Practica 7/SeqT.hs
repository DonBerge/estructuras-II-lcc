data Seq a = Empty | Node Int (Seq a) a (Seq a) deriving Show

(|||):: a -> b -> (a,b)
(|||) a b = (a,b)

size:: Seq a -> Int
size Empty = 0
size (Node s _ _ _) = s

empty :: Seq a
empty = Empty

singleton :: a -> Seq a
singleton x = Node 1 empty x empty

lengthT :: Seq a -> Int
lengthT Empty = 0
lengthT (Node l _ _ _) = l

toSeq :: [a] -> Seq a
toSeq [] = empty
toSeq (x:xs) = let 
                    rseq = toSeq xs
                in
                    Node (1+size rseq) empty x rseq

nth:: Seq a -> Int -> a
nth (Node _ Empty x _) 0 = x
nth (Node _ Empty _ r) n = nth r $ n-1
nth (Node _ l@(Node sl _ _ _) x r) n
                                | n < sl = nth l n
                                | n == sl = x
                                | otherwise = nth r (n-sl-1)

consT:: a -> Seq a -> Seq a
consT a Empty = Node 1 Empty a Empty
consT a (Node s l x r) = Node (s+1) (consT a l) x r

{-
    W(n) = 2*W(n/2)+c -> O(n)
    S(n) = S(n/2)+c -> O(log n)
-}
tabulate :: (Int -> a) -> Int -> Seq a
tabulate f = tabulate' f 0
    where
        tabulate':: (Int -> a) -> Int -> Int -> Seq a
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
mapT:: (a->b) -> Seq a -> Seq b
mapT _ Empty = Empty
mapT f (Node s l x r) = let
                            (ml,mr) = mapT f l ||| mapT f r
                        in
                            Node s ml (f x) mr

takeT :: Int -> Seq a -> Seq a
takeT _ Empty = Empty
takeT 0 _ = Empty
takeT n (Node s l x r)
                    | n <= size l = takeT n l
                    | otherwise = let 
                                    tr = takeT (n - size l - 1) r
                                  in
                                    Node (size l + 1 + size tr) l x tr 

dropT :: Int -> Seq a -> Seq a
dropT _ Empty = Empty
dropT 0 t = t
dropT n (Node s l x r)
                    | n <= size l = let
                                        dl = dropT n l
                                    in
                                        Node (size dl + 1 + size r) dl x r
                    | otherwise = dropT (n - size l - 1) r

appendT:: Seq a -> Seq a -> Seq a
appendT Empty r = r
appendT l Empty = l
appendT l r = Node (size l + size r) l (nth r 0) (dropT 1 r)

filterT :: (a -> Bool) -> Seq a -> Seq a
filterT _ Empty = Empty
filterT f (Node s l x r) =  let
                                (l',r') = filterT f l ||| filterT f r
                            in
                                if f x
                                    then
                                        appendT l' r'
                                    else
                                        Node (1 + size l' + size r') l' x r'

foldrT :: (a -> b -> b) -> b -> Seq a -> b
foldrT f e Empty = e
foldrT f e (Node _ l x r) = foldrT f (f x (foldrT f e r)) l

foldlT:: (b -> a -> b) -> b -> Seq a -> b
foldlT f e Empty = e
foldlT f e (Node _ l x r) = foldlT f (f (foldlT f e l) x) r

reduceT:: (a->a->a) -> a -> Seq a -> a
reduceT f e Empty = e
reduceT f e (Node _ Empty x Empty) = x
reduceT f e t =  let
                    m = div (lengthT t) 2
                    (l,r) = reduceT f e (takeT m t) ||| reduceT f e (dropT m t)
                in
                    f l r

fromSeq:: Seq a -> [a]
fromSeq s = reduceT (++) [] (mapT (:[]) s)