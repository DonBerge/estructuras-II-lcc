module SeqL where

(|||) :: a -> b -> (a, b)
(|||) a b = (a, b)

emptyS :: Seq a
singletonS :: a -> Seq a
toSeq :: [a] -> Seq a
nthS :: Seq a -> Int -> a
consS :: a -> Seq a -> Seq a
tabulateS :: (Int -> a) -> Int -> Seq a
appendS :: Seq a -> Seq a -> Seq a
reduceS :: (a -> a -> a) -> a -> Seq a -> a
scanS :: (a -> a -> a) -> a -> Seq a -> (Seq a, a)
lengthS :: Seq a -> Int
mapS :: (a -> b) -> Seq a -> Seq b
filterS :: (a -> Bool) -> Seq a -> Seq a
takeS :: Int -> Seq a -> Seq a
dropS :: Int -> Seq a -> Seq a
foldrS :: (a -> b -> b) -> b -> Seq a -> b
foldlS :: (b -> a -> b) -> b -> Seq a -> b

newtype Seq a = SeqImpl [a]

emptyS = SeqImpl []

singletonS t = SeqImpl [t]

lengthS (SeqImpl xs) = length xs

toSeq = SeqImpl

nthS (SeqImpl xs) n = xs !! n

consS x (SeqImpl xs) = SeqImpl $ x : xs

tabulateS f n = toSeq $ [f x | x <- [0 .. (n - 1)]]

mapS f (SeqImpl xs) = SeqImpl $ map f xs

filterS f (SeqImpl xs) = SeqImpl $ filter f xs

takeS n (SeqImpl xs) = SeqImpl $ take n xs

dropS n (SeqImpl xs) = SeqImpl $ drop n xs

foldrS f e (SeqImpl xs) = foldr f e xs

foldlS f e (SeqImpl xs) = foldl f e xs

appendS (SeqImpl xs) (SeqImpl ys) = SeqImpl $ xs ++ ys

reduceS f e (SeqImpl []) = e
reduceS f e (SeqImpl s) = f e $ head $ reduce' f s
  where
    reduce' :: (a -> a -> a) -> [a] -> [a]
    reduce' f [] = []
    reduce' f [x] = [x]
    reduce' f xs =
        let
            xs' = compact f xs
         in
            reduce' f xs'

    compact :: (a -> a -> a) -> [a] -> [a]
    compact f [] = []
    compact f [x] = [x]
    compact f (x : y : xs) = f x y : compact f xs

scanS f b (SeqImpl s) =
    let
        (s', t) = scan' s
     in
        (toSeq s', t)
  where
    scan' [] = ([], b)
    scan' [x] = ([b], f b x)
    scan' xs =
        let
            (xs', t) = scan' $ compact xs
            r = combine xs' xs
         in
            (r, t)

    compact [] = []
    compact [x] = [x]
    compact (x : y : xs) =
        let
            (x', xs') = f x y ||| compact xs
         in
            x' : xs'
    combine _ [] = []
    combine (x : _) [_] = [x]
    combine (x : xs) (y : _ : ys) =
        let
            (x', xs') = f x y ||| combine xs ys
         in
            x : x' : xs'

data TreeView a = EMPTY | ELT a | NODE (Seq a) (Seq a)

showS :: Seq a -> TreeView a
showS xs =
    let
        m = div (lengthS xs) 2
        (xl, xr) = takeS m xs ||| dropS m xs
        a = emptyS
     in
        case (xl, xr) of
            (SeqImpl [], SeqImpl []) -> EMPTY
            (_, SeqImpl []) -> ELT $ nthS xl 0
            (SeqImpl [], _) -> ELT $ nthS xr 0
            _ -> NODE xl xr

instance (Show a) => Show (Seq a) where
    show :: Seq a -> String
    show (SeqImpl s) = show s