data Treap p k = E | N (Treap p k) p k (Treap p k)

key:: Treap p k -> k
key (N _ _ k _) = k

---------------------------------

priority:: Treap p k -> p
priority (N _ p _ _) = p

---------------------------------

inorder:: Treap p k -> [k]
inorder (N l _ k r) = inorder l ++ [k] ++ inorder r

isOrdered:: Ord k => [k] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:y:xs) = x<=y && isOrdered (y:xs) 


isEmpty:: Treap p k -> Bool
isEmpty E = True
isEmpty _ = False

checkHeapProperty:: Ord p => Treap p k -> Bool
checkHeapProperty E = E
checkHeapProperty (N l p _ r) = (isEmpty l || p >= priority l) && (isEmpty r || p >= priority r) && checkHeapProperty l && checkHeapProperty r

isTreap::(Ord k, Ord p) => Treap p k -> Bool
isTreap E = True
isTreap t = (isOrdered . inorder) t && checkHeapProperty t