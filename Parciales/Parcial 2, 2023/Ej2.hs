data Tree a = E | N Int (Tree a) a (Tree a)
type Seq a = Tree a

(|||):: a -> b -> (a,b)
(|||) a b = (a,b)

len:: Seq a -> Int
len E = 0
len (N d _ _ _) = d

filterPrefix:: (a -> Bool) -> Seq a -> Seq a
filterPrefix p E = E
filterPrefix p (N _ l x r) = let
                                (prefixL, prefixR) = filterPrefix p l ||| filterPrefix p r
                             in
                                if (len prefixL == len l) && p x 
                                    then
                                        N (len prefixL + 1 + len prefixR) prefixL x prefixR
                                    else
                                        prefixL
-- W(n) = W(l)+w(r)+c
-- S(n) = max{S(l),S(r)}+c