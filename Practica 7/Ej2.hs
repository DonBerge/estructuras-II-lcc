import SeqL 
matMult:: Num a => (a,a,a,a) -> (a,a,a,a) -> (a,a,a,a)
matMult (a0,a1,a2,a3) (b0,b1,b2,b3) = (a0*b0+a1*b2, a0*b1+a1*b3, a2*b0+a3*b2, a2*b1+a3*b3)

fibSeq:: Int -> Seq Int
fibSeq n = mapS (\(a,_,_,_) -> a) $ fst $ scanS matMult (1,0,0,1) $ tabulateS (const (1,1,1,0)) n