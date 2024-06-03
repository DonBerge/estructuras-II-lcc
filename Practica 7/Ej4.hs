import SeqL

data Paren = Open | Close deriving Show

toParen::String -> Seq Paren
toParen = mapS convert . toSeq
    where
        convert '(' = Open
        convert ')' = Close

{-
    W(n)=O(n)
    S(n)=O(lg n)
-}
matchParen :: Seq Paren -> Bool
matchParen s = matchP s == (0,0)
  where
    val:: (Int,Int)
    val = (0, 0)

    base:: Paren -> (Int,Int)
    base Open = (0, 1)
    base Close = (1, 0)

    combine:: (Int,Int) -> (Int,Int) -> (Int, Int)
    combine (a, b) (c, d) = (a + max 0 (c - b), d + max 0 (b - c))

    matchP s = reduceS combine (0,0) $ mapS base s