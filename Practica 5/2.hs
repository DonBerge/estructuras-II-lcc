class Pila t where
    empty:: t a
    push:: a -> t a -> t a

    isEmpty:: t a -> Bool
    isEmpty empty = True
    isEmpty (push x q) = False

    top:: t a -> a
    pop:: t a -> t a


