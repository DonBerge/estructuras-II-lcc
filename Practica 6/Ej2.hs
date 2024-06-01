data Tree a = E | Leaf a | Join (Tree a) (Tree a)

mapreduce:: (a->b) -> (b->b->b) -> b -> Tree a -> b 
mapreduce f g e = mr
    where
        mr E = e
        mr (Leaf a) = f a
        mr (Join l r) = let (l',r') = (mr l, mr r) in g l' r'
{-
findSums::(Num a, Ord a) => Tree a -> (a, a, a, a)
findSums E = (0,0,0,0)
findSums (Leaf v) = (max v 0, max v 0, max v 0, v)
findSums (Join l r) = let
                        (
                            (cl,pl,sl,tl),
                            (cr,pr,sr,tr)
                            ) = (findSums l, findSums r) 
                        in 
                            (sl+pr,pl,sr,tl+tr)

maximum4::(Ord a) => (a,a,a,a) -> a
maximum4 (a,b,c,d) = max a $ max b $ max c d
-}

maximum4::(Ord a) => (a,a,a,a) -> a
maximum4 (a,b,c,d) = max a $ max b $ max c d

transformElement::(Num v, Ord v) => v -> (v,v,v,v)
transformElement v = (max v 0, max v 0, max v 0, v)

calculateSums:: (Num v, Ord v) => (v,v,v,v) -> (v,v,v,v) -> (v,v,v,v)
calculateSums (cl,pl,sl,tl) (cr,pr,sr,tr) = (
    -- Maxima suma contigua = maximo entre una suma contigua de l, una suma contigua de r y una suma contigua que incluya elementos de ambos lados
    max (sl+pr) $ max cl cr,
    -- Maxima suma de un prefijo
    max pl (tl+pr),
    -- Maxima suma de un sufijo
    max sr (sl+tr),
    -- Maxima suma total
    tl+tr)

mcss:: (Num a, Ord a) => Tree a -> a
mcss = maximum4 . mapreduce transformElement calculateSums (0,0,0,0)

t = (Leaf (-1) `Join` Leaf (-2) `Join` Leaf (-3) `Join` Leaf (-4))