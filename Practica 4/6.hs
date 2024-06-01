import LeftistHeap

--fromList:: Ord a => [a] -> Heap a
--fromList = foldr insert E

fromList:: Ord a => [a] -> Heap a
fromList = head . fromList' . map (\a-> N 1 a E E)
    where
        fromList':: Ord a => [Heap a] -> [Heap a]
        fromList' [] = [E]
        fromList' [x] = [x]
        fromList' xs = fromList'(mergePairs xs)

        mergePairs:: Ord a => [Heap a] -> [Heap a]
        mergePairs [] = []
        mergePairs [x] = [x]
        mergePairs (x:y:xs) = merge x y : fromList' xs