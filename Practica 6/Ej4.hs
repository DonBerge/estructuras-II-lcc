data T a = E | N (T a) a (T a)
altura :: T a -> Int
altura E = 0
altura (N l x r ) = 1 + max (altura l) (altura r)

combinar:: T a -> T a -> T a
combinar E t2 = t2
combinar (N l x r) t2 = N (combinar l r) x t2

filterT:: (a -> Bool) -> T a -> T a
filterT p E = E
filterT p (N l x r) = if p x
                        then combinar (filterT p l) (filterT p r)
                        else N (filterT p l) x (filterT p r)

quicksort:: Ord a => T a -> T a
quicksort E = E
quicksort t@(N l x r) = N men x mas
                        where
                            (men,mas) = (quicksort $ filterT (<x) t, quicksort $ filterT (>x) t)