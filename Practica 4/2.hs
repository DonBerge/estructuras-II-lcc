data BST a = E | Node (BST a) a (BST a) deriving Show

isSorted:: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x<=y && isSorted(y:xs)


inorder'':: BST a -> [a]
inorder'' E = []
inorder'' (Node l a r) = inorder l ++ [a] ++ inorder r

inorder:: BST a -> [a]
inorder = flip inorder' []
    where
        inorder':: BST a -> [a] -> [a]
        inorder' E = id
        inorder' (Node l a r) = inorder' l . (a:) . inorder' r

maximumBST:: Ord a => BST a -> a
maximumBST (Node E a E) = a
maximumBST (Node l a E) = max a $ maximumBST l
maximumBST (Node E a r) = max a $ maximumBST r
maximumBST (Node l a r) = max a $ max (maximumBST l) (maximumBST r)

checkBST:: Ord a => BST a -> Bool
checkBST = isSorted . inorder

member:: Ord a => a -> BST a -> Bool
member = member' Nothing
    where
        member':: Ord a => Maybe a -> a -> BST a -> Bool
        member' Nothing _ E = False
        member' (Just b) a E = a==b
        member' candidate a (Node l b r)
                        |   a < b = member' candidate a l
                        |   otherwise = member' (Just b) a r 


insert :: Ord a => a -> BST a -> BST a
insert a E = Node E a E
insert a (Node l b r) 
                | a <= b = Node (insert a l) b r
                | otherwise = Node l b (insert a r)

splitBST:: Ord a => BST a -> a -> (BST a, BST a)
splitBST E _ = (E,E)
splitBST (Node l b r) a
        |   a < b = let (lta, gta) = splitBST l a in (lta, Node gta b r)
        |   otherwise = let (lta, gta) = splitBST r a in (Node l b lta, gta)
                    

joinBST:: Ord a => BST a -> BST a -> BST a
joinBST E = id
joinBST (Node l x r) = joinBST r . joinBST l . insert x


t :: BST Int
t = Node (Node (Node E 1 E) 2 E) 10 (Node (Node E 15 E) 20 E)