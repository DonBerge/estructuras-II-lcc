import Ej1

splitAtT:: BTree a -> Int -> (BTree a, BTree a)
splitAtT t n = takeT n t ||| dropT n t

rebalance:: BTree a -> BTree a
rebalance t = let
                (l',r) = splitAtT t (div (size t) 2)
                (l, Node _ _ m _) = splitAtT l' (size l' - 1)
                (rl, rr) = rebalance l ||| rebalance r
              in
                Node (size rl + 1 + size rr) rl m rr