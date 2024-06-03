module Ej7 where
import SeqL

-- W(n,m) = n+m, S(n,m) = O(lg n * lg m)
merge::(a->a->Ordering) -> Seq a -> Seq a -> Seq a
merge ord xs ys
        | lengthS xs == 0 = ys
        | otherwise = let
                        -- O(1)
                        pivot = nthS xs (lengthS xs `div` 2)

                        -- Para cada uno de estos, W = O(n), S = O(lg n)
                        (xless,yless) = filterS ((==GT) . ord pivot) xs ||| filterS ((==GT) . ord pivot) ys
                        (xgreat,ygreat) = filterS ((==LT) . ord pivot) xs ||| filterS ((==LT) . ord pivot) ys
                        (xeq,yeq) = filterS ((==EQ) . ord pivot) xs ||| filterS ((==EQ) . ord pivot) ys
                      in
                        -- W = O(n+m), S=O(1)
                        merge ord xless yless `appendS` xeq `appendS` yeq `appendS` merge ord xgreat ygreat

-- W(n) = n lg n , S(n) = lg^2 n
sort:: (a->a->Ordering) -> Seq a -> Seq a
sort ord = reduceS (merge ord) emptyS . mapS singletonS

-- W=O(n), S=O(lg n)
maxE :: (a -> a -> Ordering) -> Seq a -> a
maxE ord xs = reduceS combine val xs
  where
    val = nthS xs 0
    combine a b = if ord a b == GT then a else b

-- W=O(n), S=O(lg n)
maxS :: (a -> a -> Ordering) -> Seq a -> Int
maxS ord xs =
    let
        elementos_indices = tabulateS (\i -> (nthS xs i, i)) $ lengthS xs
        maxes = maxE comparar elementos_indices
     in
        snd maxes
  where
    comparar (a, i) (b, j) = let compE = ord a b in if compE == EQ then compare i j else compE

-- W(n) = n lg n, S(n) = lg n
-- Capaz se puede hacer mejor con scan
group:: (a -> a -> Ordering) -> Seq a -> Seq a
group ord xs = reduceS combine emptyS $ mapS singletonS xs
    where
        combine xs ys
                    | lengthS xs == 0 = ys
                    | lengthS ys == 0 = xs
                    | otherwise =
                        let
                            -- O(1)
                            nXS = lengthS xs
                            -- O(1)
                            (lastXS,headYS) = nthS xs (nXS-1) ||| nthS ys 0
                        in
                            case ord lastXS headYS of
                                -- W(n,m) = O(n+m), S(n,m) = O(1)
                                EQ -> appendS xs $ dropS 1 ys
                                _ -> appendS xs ys

-- W=O(n lg n), S=O(lg n)
collect::Ord a => Seq (a,b) -> Seq (a, Seq b)
collect xs = let
                    ord (a,b) (c,d) = compare a c
                    -- W = O(n lg n), S = O(lg^2 n)
                    sorted_xs = sort ord xs
             in
                    -- W = O(n lg n), S = O(lg n)
                    reduceS combine emptyS $ mapS (\(a,b) -> singletonS (a, singletonS b)) sorted_xs
             where
                combine xs ys 
                            | lengthS xs == 0 = ys
                            | lengthS ys == 0 = xs
                            | otherwise = let
                                            -- O(1)
                                            (lastXS, headYS) = nthS xs (lengthS xs -1) ||| nthS ys 0
                                          in
                                            -- W=Wc, S=Sc
                                            if fst lastXS == fst headYS
                                                -- W=O(n), S=O(1)
                                                then takeS (lengthS xs - 1) xs `appendS` singletonS (fst lastXS, snd lastXS `appendS` snd headYS) `appendS` dropS 1 ys
                                                else xs `appendS` ys