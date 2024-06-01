type Interval = (Int,Int)
data ITree = E | N ITree Interval ITree deriving Show

right:: ITree -> Int
right (N _ (_,b) E) = b
right (N _ _ r) =  right r

-------------------------------------

isEmpty:: ITree -> Bool
isEmpty E = True
isEmpty _ = False

left:: ITree -> Int
left (N E (a,_) _) = a
left (N l _ _) = left l

checkIT:: ITree -> Bool
checkIT E = True
checkIT (N l (a,b) r) = a<=b && 
                        checkIT l && 
                        checkIT r && 
                        (isEmpty l || right l < a-1) && 
                        (isEmpty r || b+1 < left r)

-------------------------------------

splitMax:: ITree -> (Interval, ITree)
splitMax (N l i E) = (i, l)
splitMax (N l i r) = let (rinter, rtree)=splitMax r in (rinter, N l i rtree)

---------------------------------------------------------------------------------

insertInterval:: Interval -> ITree -> ITree
insertInterval i E = N E i E
insertInterval i@(a,b) t@(N l j@(c,d) r)
                                    | a < c = N (insertInterval i l) j r
                                    | otherwise = N l j (insertInterval i r)

merge:: ITree -> ITree -> ITree
merge l E = l
merge E r = r
merge l r = let (rinter, rtree)=splitMax r in merge (insertInterval rinter l) rtree

---------------------------------------------------------------------------------
delTree:: Int -> ITree -> ITree
delTree _ E = E
delTree i (N l (a,b) r)
                    |   i == a = if a==b then merge l r else N l (a-1,b) r
                    |   i == b = if a==b then merge l r else N l (a,b-1) r
                    |   a < i && i < b = merge (N l (a,i-1) E) (N E (i+1,b) r)
                    |   otherwise = if i < a then N (delTree i l) (a,b) r else N l (a,b) (delTree i r)

t :: ITree
t = N (N E (1,3) E) (5,9) (N E (15,20) E)