data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

mapreduce:: (a->b) -> (b->b->b) -> b -> Tree a -> b 
mapreduce f g e = mr
    where
        mr E = e
        mr (Leaf a) = f a
        mr (Join l r) = let (l',r') = (mr l, mr r) in g l' r'

sufijos:: Tree Int -> Tree (Tree Int)
sufijos E = E
sufijos (Leaf _) = Leaf E
sufijos (Join l r) = Join (Leaf r) (sufijos r)

t:: Tree Int
t = Join (Join (Leaf 10) (Leaf 15)) (Leaf 20)
