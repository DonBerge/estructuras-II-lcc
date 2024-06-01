import SeqL

promedios:: Seq Int -> Seq Float
promedios s = let
                  -- Lista de pares de numeros, la primer componente es el numero de elementos sumados
                  -- la segunda es la suma total
                  pares = mapS ((1,) . fromIntegral) s
                  (x,xs) = nthS pares 0 ||| dropS 1 pares
                  (ys,y) = scanS (\(a,b) (c,d) -> (a+c,b+d)) x xs

                  -- Divido la suma total por la cantidad de elementos
                  ys' = mapS (uncurry $ flip (/)) (appendS ys $ singletonS y)
                  in
                    ys'

{-
mayores:: Seq Int -> Int
mayores emptyS = 0
mayores s = let
               (x,xs) = nthS s 0 ||| dropS 1 s 
               (s',_) = scanS max x xs
               s = zip s' xs
            in
               lengthS $ filterS id $ mapS (uncurry (<)) s
-}