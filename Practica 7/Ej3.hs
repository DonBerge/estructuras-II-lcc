import SeqL 

reverseS:: Seq a -> Seq a
reverseS s = let
                n = lengthS s
             in
                tabulateS (\i -> nthS s (n-1-i)) n

aguaHist:: Seq Int -> Int
aguaHist h = let
                (maxL,maxR) = fst (scanS max 0 h) ||| reverseS (fst (scanS max 0 $ reverseS h))
             in
                reduceS (+) 0 $ tabulateS (\i -> 
                    max 0 $ min (nthS maxL i) (nthS maxR i) - nthS h i
                    ) $ lengthS h
                    
hist :: Seq Int
hist = toSeq [2,3,4,7,5,2,3,2,6,4,3,5,2,1]