module Group

interface Group a where
    times : a -> a -> a

    inv : a -> a
    e : a

Group Int where
    times m n = m + n
    inv m = m * -1
    e = 0