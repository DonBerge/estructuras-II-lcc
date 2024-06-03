import SeqL

-- W(n) = n^2, S(n) = lg n
cantMultiplos:: Seq Int -> Int
cantMultiplos s = let
                    -- O(1)
                    n = lengthS s
                    -- W=O(i), S=(lg i)
                    computar i = let
                                    js = takeS i s -- O(1)
                                    multiplos = filterS (\x -> mod x (nthS s i) == 0) js -- W=O(i), S=(lg i)
                                 in
                                    lengthS multiplos -- O(1)
                    
                    -- W=O(n^2), S=(lg n)
                    calcularLargos = tabulateS computar n
                  in
                    -- W=O(n), S=(lg n)
                    reduceS (+) 0 calcularLargos