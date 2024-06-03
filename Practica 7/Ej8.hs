import SeqL
import Ej7 (maxE, collect)

join:: Seq (Seq a) -> Seq a
join = reduceS appendS emptyS

mapCollectReduce apv red s = let 
                                pairs = join (mapS apv s)
                                groups = collect pairs
                             in mapS red groups

promedio:: Seq Int -> Float
promedio xs = reduceS (+) 0 (mapS fromIntegral xs) / fromIntegral (lengthS xs)

apv:: (String, Seq Int) -> Seq (Int,Int)
apv xs = let
            prom = promedio $ snd xs
            clave
                | prom >= 70 = 1
                | prom >  50 = 2
                | otherwise = 3
        in
            singletonS (clave, maxE compare $ snd xs)

red:: (Int,Seq Int) -> (Int,Int)
red = (\t -> (lengthS t, maxE compare t)) . snd

datosIngreso:: Seq (String, Seq Int) -> Seq (Int, Int)
datosIngreso = mapCollectReduce apv red